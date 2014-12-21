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

  def printable[A] : Printable[A] = macro mkPrintable[A]

  def fmtTypeName(c: blackbox.Context)(aType:c.Type) : String = {
    if(isTupleType(c)(aType)) {
      ""
    } else {
      aType.typeSymbol.name.toString
    }
  }

  def fmtSymbol(c: blackbox.Context)(aType:c.Type,optSymbol:Option[c.Symbol]) : String = {
    if(isTupleType(c)(aType)) {
      ""
    } else {
      optSymbol.map(_.name.toString + "=").getOrElse("")
    }
  }

  def mkPrintable[A: c.WeakTypeTag](
    c: blackbox.Context
  ): c.Expr[Printable[A]] = {
    import c.universe._

    val printableTypeConstructor = typeOf[Printable[_]].typeConstructor
    val aType = c.weakTypeOf[A]

    val oomTypeSig = calcStructType(c)(aType)

//    val isTuple = isTupleType(c)(aType)
    val body =
      if(oomTypeSig.size == 1) {
        val (optSymbol, _type) = oomTypeSig.head
        val innerPrintableType = appliedType(printableTypeConstructor, List(_type))
        val innerPrintable = inferImplicitOrDie(c)(innerPrintableType)
        q"""
val v = ${aType.typeSymbol.companion}.unapply(a).get
${fmtTypeName(c)(aType)} + "(" +
${fmtSymbol(c)(aType,optSymbol)} + $innerPrintable.print(v) +
")"
        """
      } else {
        val values =
          oomTypeSig
            .zipWithIndex
            .map { case ((optSymbol, _type), i) =>
              val innerPrintableType = appliedType(printableTypeConstructor, List(_type))
              val innerPrintable = inferImplicitOrDie(c)(innerPrintableType)
              q"""${fmtSymbol(c)(aType,optSymbol)} + $innerPrintable.print(tuple.${TermName("_" + (i+1))})"""
            }
        q"""
val tuple = ${aType.typeSymbol.companion}.unapply(a).get
${fmtTypeName(c)(aType)} + "(" +
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


  /**
   * Infer an implicit value for a type. Aborts if the implicit value does not
   * exist.
   * @param c context
   * @param aType type to infer implicit value for
   * @return tree of implicit value
   */
  def inferImplicitOrDie(
    c: blackbox.Context
  )(
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
   * @param c context
   * @param aType type whose companion object should be searched for method
   * @return the matching method symbol
   */
  def getCompanionMethodOrDie(
    c: blackbox.Context
  )(
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
  def isTupleType(c: blackbox.Context)(aType:c.Type) : Boolean = {
    tupleNamePattern.matcher(aType.typeSymbol.fullName.toString).matches
  }

  /**
   * Compute the struct type (a non-empty list of (Symbol,Type) pairs) for a
   * type that can be used to both create an instance of the type and 
   * decompose the time into its component data or struct types.
   *
   * For case classes, the struct type is the list of (field name, field type)
   * pairs of the case class.
   * For tuples, the struct type is the (incremental tuple field name ("_1",
   * "_2", etc), field type) pairs.
   * For other types, the struct type equals the first unapply/apply
   * method pair with matching struct types in the type's companion object.
   * Symbol names are extracted from the matching apply method arguments.
   *
   * The struct types of apply/unapply match when either:
   * 1) The types of the arguments to the apply method equal the option/tuple
   * extracted return type arguments of the unapply method (extraction here
   * removes  the Option and Tuple wrappers)
   * Ex:
   * case class A(value1: int, value2: Int)
   * A.apply(value1: Int,value2: Int) "matches" A.unapply(a: A) : Option[(Int,Int)]
   * 2) The type of the single argument to the apply method matches the
   * option extracted return type of the unapply method (extraction here removes
   * only the Option wrapper)
   * Ex:
   * case class A(value1: (Int,Int))
   * A.apply(value1: (Int,Int)) "matches" A.unapply(a: A) : Option[(Int,Int)]
   *
   * Correctly computes struct types of single value case classes,
   * single value tuple case classes and any other type that declares an
   * apply/unapply pair with matching struct types in the type's
   * companion object.
   *
   * @param c context
   * @param aType type whose companion object should be searched for apply and
   *              unapply methods
   * @return the struct type as a non-empty list of (Symbol,Type) pairs
   */
  def calcStructType(
    c: blackbox.Context
  )(
    aType: c.Type
  ): List[(Option[c.Symbol], c.Type)] = {
    import c.universe._

    val unapplyMethod = getCompanionMethodOrDie(c)(aType,"unapply")
    val oomUnapplyMethodStructType =
      calcOomUnapplyStructType(c)(aType,unapplyMethod)

    val oomApplyMethodAlt =
      getCompanionMethodOrDie(c)(aType,"apply")
        .asTerm
        .alternatives
        .map(_.asMethod)

    val oomApplyMethodStructType =
      oomApplyMethodAlt.map { alt =>
        calcApplyStructType(c)(aType, alt)
      }

    // Search for first unapply struct type that matches an apply struct type
    oomUnapplyMethodStructType.toStream.map { unapplyStructType =>
      oomApplyMethodStructType.find { applyStructType =>
        isMatchStructType(c)(unapplyStructType, applyStructType)
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
        oomApplyMethodStructType.map(printStructType(c)).mkString("\n  ") +
        s"Found unapply struct type:\n" +
        oomUnapplyMethodStructType.map(printStructType(c)).mkString("\n  ")
      )
    }
  }

  trait MyContext extends blackbox.Context {

  }

  def printStructType(
    c:blackbox.Context
  )(
    structType: List[(Option[c.Symbol],c.Type)]
  ) : String = {
    structType.map { case (optSymbol, _type) =>
      optSymbol.fold("")(symbol => s"$symbol=") + _type.typeSymbol.fullName
    }.mkString(",")
  }

  def isMatchStructType(
    c:blackbox.Context
  )(
    lhs: List[(Option[c.Symbol],c.Type)],
    rhs: List[(Option[c.Symbol],c.Type)]
  ) : Boolean = {
    lhs.corresponds(rhs)(_._2 == _._2)
  }

  def filterMethodStructType(
    c:blackbox.Context
  )(
    aType:c.Type,
    method:c.universe.MethodSymbol
  )(
    calcOomStructType: => List[List[(Option[c.Symbol],c.Type)]]
  ) : List[List[(Option[c.Symbol], c.Type)]] = {
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
        // ??? is the comparing the fullName of two symbols the same as == ?
        // ??? does comparing fullName work with generics?
        val methodTypeParamToTypeParam =
          method.typeParams
            .map(_.fullName)
            .zip(aType.typeArgs)
            .toMap
        calcOomStructType.map { structType =>
          structType.map { case orig@(optSymbol, _type) =>
            methodTypeParamToTypeParam.get(_type.typeSymbol.fullName) match {
              case Some(translatedType) =>
                (optSymbol, translatedType)
              case None => orig
            }
          }
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
    c:blackbox.Context
  )(
    aType:c.Type,
    applyMethod:c.universe.MethodSymbol
  ) : List[(Option[c.Symbol], c.Type)] = {
    filterMethodStructType(c)(aType,applyMethod) {
      List(
        applyMethod.paramLists.head.map { param =>
        (Some(param),param.typeSignature)
        }
      )
    }.headOption.getOrElse(Nil)
  }

  def calcOomUnapplyStructType(
    c:blackbox.Context
  )(
    aType:c.Type,
    unapplyMethod:c.universe.MethodSymbol
  ) : List[List[(Option[c.Symbol], c.Type)]] = {
    import c.universe._

    filterMethodStructType(c)(aType,unapplyMethod) {
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
             isTupleType(c)(tupleType) =>
          List(List((None,tupleType)), innerArgs.map(a => (None, a)))
        // For anything else there is only one possible struct type
        case typeRef@TypeRef(_, _, _) =>
          List(List((None,typeRef)))
      }
    }
  }

}
