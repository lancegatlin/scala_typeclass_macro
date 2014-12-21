package org.lancegatlin

import scala.language.experimental.macros
import scala.reflect.macros.blackbox

object Macro {

  def debugMacro(c: blackbox.Context)(s: c.Expr[Any]) : c.Expr[Unit] = {
    import c.universe._
    val paramRep = show(s.tree)
    c.Expr(q"""println($paramRep + " = " + $s)""")
  }


  def debug(s: Any) : Unit = macro debugMacro

  def printable[A] : Printable[A] = macro printableMacro[A]

  def printableMacro[A:c.WeakTypeTag](c: blackbox.Context) : c.Expr[Printable[A]] = {
    val builder = new PrintableMacroBuilder(c)
    // TODO: why is this cast necessary?
    builder.build[A].asInstanceOf[c.Expr[Printable[A]]]
  }

  class PrintableMacroBuilder(val c: blackbox.Context) extends MacroToolbox {
    import c.universe._
    
    def build[A: c.WeakTypeTag] : c.Expr[Printable[A]] = {
      val aType = c.weakTypeOf[A]
      val isTuple = isTupleType(aType)

      val typeName = {
        if(isTuple) {
          ""
        } else {
          aType.typeSymbol.name.toString
        }
      }

      def fmtSymbol(optSymbol:Option[c.Symbol]) : String = {
        if(isTuple) {
          ""
        } else {
          optSymbol.fold("")(_.name.toString + "=")
        }
      }

      val printableTypeConstructor = typeOf[Printable[_]].typeConstructor

      val structType = calcStructType(aType)
  
      val body =
        if(structType.oomMember.size == 1) {
          val (optSymbol, _type) = structType.oomMember.head
          val innerPrintableType = appliedType(printableTypeConstructor, List(_type))
          val innerPrintable = inferImplicitOrDie(innerPrintableType)
          q"""
val v = ${aType.typeSymbol.companion}.unapply(a).get
$typeName + "(" +
${fmtSymbol(optSymbol)} + $innerPrintable.print(v) +
")"
          """
        } else {
          val values =
            structType.oomMember
              .zipWithIndex
              .map { case ((optSymbol, _type), i) =>
                val innerPrintableType = appliedType(printableTypeConstructor, List(_type))
                val innerPrintable = inferImplicitOrDie(innerPrintableType)
                q"""${fmtSymbol(optSymbol)} + $innerPrintable.print(tuple.${TermName("_" + (i+1))})"""
              }
          q"""
val tuple = ${aType.typeSymbol.companion}.unapply(a).get
$typeName + "(" +
Seq(..$values).mkString(",") +
")"
          """
        }
  
      c.Expr[Printable[A]] {
        q"""
  new Printable[$aType] {
    def print(a: $aType) = $body
  }
        """
      }
    }
  }  
    

  trait MacroToolbox {
    val c: blackbox.Context
    
    import c.universe._
    
    /**
     * Infer an implicit value for a type. Aborts if the implicit value does not
     * exist.
     * @param aType type to infer implicit value for
     * @return tree of implicit value
     */
    def inferImplicitOrDie(
      aType:c.Type
    ) : c.Tree = {
      val result = c.inferImplicitValue(aType)
      if (result.isEmpty) {
        c.abort(c.enclosingPosition, s"Implicit $aType does not exist")
      }
      result
    }
    
    /**
     * Get a symbol to a method in a type's companion object within a
     * context. Aborts if method doesn't exist.
     * @param aType type whose companion object should be searched for method
     * @return the matching method symbol
     */
    def getCompanionMethodOrDie(
      aType: c.Type,
      methodName: String
    ) : c.universe.MethodSymbol = {
      import c.universe._
      aType.typeSymbol.companion.typeSignature.decl(TermName(methodName)) match {
        case NoSymbol => 
          c.abort(
            c.enclosingPosition,
            s"$aType.$methodName method does not exist"
          )
        case s => s.asMethod
      }
    }
  
    val tupleNamePattern = "scala\\.Tuple\\d{1,2}".r.pattern
    def isTupleType(aType:c.Type) : Boolean = {
      tupleNamePattern.matcher(aType.typeSymbol.fullName.toString).matches
    }

    /**
     * A case class that represents the struct type for some type. A struct type
     * contains a list of the (Symbol,Type) pairs that represent the data types
     * (or struct types) necessary to both create an instance of the type
     * (apply) and capture the data returned when an instance of the type is
     * decomposed (unapply).
     * @param oomMember one or more (Symbol,Type) pairs
     */
    case class StructType(oomMember: List[(Option[c.Symbol], c.Type)]) {
      require(oomMember.nonEmpty)

      def print : String = {
        oomMember.map { case (optSymbol, _type) =>
          optSymbol.fold("_:")(symbol => s"$symbol:") + _type.typeSymbol.fullName
        }.mkString(",")
      }

      /**
       * Test if two struct match. Two struct types match if their member types
       * correspond exactly.
       * @param other other struct type to test
       * @return TRUE if struct types match
       */
      def matches(other: StructType) : Boolean = {
        oomMember.corresponds(other.oomMember)(_._2 == _._2)
      }
    }
    
    /**
     * Compute the struct type for a type
     *
     * For case classes, the struct type is the list of (field name, field type)
     * pairs of the case class.
     * For tuples, the struct type is the (incremental tuple field name ("_1",
     * "_2", etc), field type) pairs of the tuple.
     * For other types, the struct type equals the first unapply/apply method
     * pair  in the type's companion object with matching struct types.
     *
     * @param aType type whose companion object should be searched for apply and
     *              unapply methods
     * @return the struct type that contains a non-empty member list of
     *         (Symbol,Type) pairs
     */
    def calcStructType(
      aType: c.Type
    ): StructType = {
      def filterMethodStructType(
        aType:c.Type,
        method:c.universe.MethodSymbol
      )(
        calcOomStructType: => List[StructType]
      ) : List[StructType] = {
        if(
          // Silently ignore methods who have a different count of type parameters
          // than the type
          method.typeParams.size == aType.typeConstructor.typeParams.size &&
          // Silently ignore curried methods
          method.paramLists.size == 1
        ) {
          // Ignore but warn about methods whose type parameters are exact match
          // size but whose symbols don't match the type's type parameter symbols
          // exactly. Using just apply/unapply its not possible to figure out
          // how the type parameters align to the type's type parameter without
          // just assuming that they align exactly in the same order.
          /*
            Apply method can have different symbols in different order than type's
            type parameters:
            class A[X,Y](value1: X, value2: Y)
            object A {
              // compiles but possible weirdness in macro here! so just ignore it and warn
              def apply[BB,AA](value1: AA, value2: BB) =
                new A(value1,value2)
            }
           */
          if(method.typeParams.map(_.toString) == aType.typeConstructor.typeParams.map(_.toString)) {
            // TODO: is the comparing the fullName of two symbols the same as == ?
            // TODO: does comparing fullName work with generics?
            val methodTypeParamToTypeParam =
              method.typeParams
                .map(_.fullName)
                .zip(aType.typeArgs)
                .toMap
            calcOomStructType.map { structType =>
              // Translate member generic type parameters to something bound
              // TODO: figure out normal way this subst is supposed to be done - prob doesn't work for bound generic types
              StructType(
                structType.oomMember.map { case orig@(optSymbol, _type) =>
                  methodTypeParamToTypeParam.get(_type.typeSymbol.fullName) match {
                    case Some(translatedType) =>
                      (optSymbol, translatedType)
                    case None => orig
                  }
                }
              )
            }
          } else {
            c.warning(
              c.enclosingPosition,
              s"Ignoring possible matching $method method whose type parameter symbols ${method.typeParams} does not match ${aType.typeSymbol} type parameter symbols ${aType.typeConstructor.typeParams}"
            )
            Nil
          }
        } else {
            c.warning(
              c.enclosingPosition,
              s"Ignoring possible matching $method method ${method.paramLists.size} ${method.typeParams.size} ${aType.typeParams.size}"
            )
          Nil
        }
    
      }
    
      def calcApplyStructType(
        aType:c.Type,
        applyMethod:c.universe.MethodSymbol
      ) : StructType = {
        filterMethodStructType(aType,applyMethod) {
          List(
            StructType(
              applyMethod.paramLists.head.map { param =>
                (Some(param),param.typeSignature)
              }
            )
          )
        }.head
      }
    
      def calcOomUnapplyStructType(
        aType:c.Type,
        unapplyMethod:c.universe.MethodSymbol
      ) : List[StructType] = {
        import c.universe._
    
        filterMethodStructType(aType,unapplyMethod) {
          // Outer type for unapply is always Option, so strip it
          val TypeRef(_,_,oneOuterArg) = unapplyMethod.returnType
          oneOuterArg.head match {
             /*
              if the unapply methods returns Option[TupleX[A,B..Z]] then there are
              two possible struct types (matching apply method determines which
              is correct):
              1) _:TupleXX[A,B,..Z]
              2) _:A,_:B,.._:Z
              */
            case tupleType@TypeRef(_, symbol, innerArgs)
              if innerArgs.nonEmpty &
                 isTupleType(tupleType) =>
              List(StructType(List((None,tupleType))), StructType(innerArgs.map(a => (None, a))))
            // For anything else there is only one possible struct type
            case typeRef@TypeRef(_, _, _) =>
              List(StructType(List((None,typeRef))))
          }
        }
      }
  
      val unapplyMethod = getCompanionMethodOrDie(aType,"unapply")
      val oomUnapplyMethodStructType =
        calcOomUnapplyStructType(aType,unapplyMethod)
  
      val oomApplyMethodAlt =
        getCompanionMethodOrDie(aType,"apply")
          .asTerm
          .alternatives
          .map(_.asMethod)
  
      val oomApplyMethodStructType =
        oomApplyMethodAlt.map { alt =>
          calcApplyStructType(aType, alt)
        }
  
      // Search for first unapply struct type that matches an apply struct type
      oomUnapplyMethodStructType.toStream.map { unapplyStructType =>
        oomApplyMethodStructType.find { applyStructType =>
          unapplyStructType matches applyStructType
        } match {
          case Some(applyStructType) =>
            Some(applyStructType)
          case None => None
        }
      }.collectFirst { case optStructType if optStructType.nonEmpty =>
        optStructType.get
      }.getOrElse {
        c.abort(
          c.enclosingPosition,
          s"No matching apply/unapply method pair found for ${aType.typeSymbol.fullName}\n" +
          s"Found ${oomApplyMethodAlt.size} apply methods:\n" +
          oomApplyMethodStructType.map(_.print).mkString("\n  ") +
          s"Found unapply struct type:\n" +
          oomUnapplyMethodStructType.map(_.print).mkString("\n  ")
        )
      }
    }
  
  }



}
