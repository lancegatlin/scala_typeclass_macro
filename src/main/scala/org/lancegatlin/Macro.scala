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

  def mkPrintable[A: c.WeakTypeTag](
    c: blackbox.Context
  ): c.Expr[Printable[A]] = {
    import c.universe._

    val printableTypeConstructor = typeOf[Printable[_]].typeConstructor
    val aType = c.weakTypeOf[A]

    val oomTypeSig = calcTypeSig(c)(aType)

    val values =
      if(oomTypeSig.size == 1) {
        val (symbol, _type) = oomTypeSig.head
        val innerPrintableType = appliedType(printableTypeConstructor, List(_type))
        val innerPrintable = inferImplicitOrDie(c)(innerPrintableType)
        q"""${symbol.name.toString} + "=" + $innerPrintable.print(tuple)""" :: Nil
      } else {
        oomTypeSig
          .zipWithIndex
          .map { case ((symbol, _type), i) =>
            val innerPrintableType = appliedType(printableTypeConstructor, List(_type))
            val innerPrintable = inferImplicitOrDie(c)(innerPrintableType)
            q"""${symbol.name.toString} + "=" + $innerPrintable.print(tuple.${TermName("_" + (i+1))})"""
          }
      }

    c.Expr[Printable[A]] {
      q"""
new Printable[$aType] {
  def print(a: $aType): String = {
    val tuple = ${aType.typeSymbol.companion}.unapply(a).get
    ${aType.typeSymbol.name.toString} + "(" +
    Seq(..$values).mkString(",") +
    ")"
  }
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

  /**
   * Compute the type signature (a non-empty list of (Symbol,Type) pairs) for a
   * type that can be used to both create and extract a type. The type signature
   * is used to describe the data contained in the type.
   *
   * For case classes, the type signature matches the fields of the case
   * class. For other types, the type signature equals the first unapply/apply
   * method pair with matching type signatures in the type's companion object.
   * Symbol names are extracted from the matching apply method arguments.
   *
   * The type signatures of apply/unapply match when either:
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
   * Correctly computes type signatures of single value case classes,
   * single value tuple case classes and any other type that declares an
   * apply/unapply pair with matching type signatures in the type's
   * companion object.
   *
   * @param c context
   * @param aType type whose companion object should be searched for apply and
   *              unapply methods
   * @return the type signature as a non-empty list of (Symbol,Type) pairs
   */
  def calcTypeSig(
    c: blackbox.Context
  )(
    aType: c.Type
  ): List[(c.Symbol, c.Type)] = {
    import c.universe._

    val unapplyMethod = getCompanionMethodOrDie(c)(aType,"unapply")

    // Build list of candidate type sigs from unapply method return type sigs
    val oomCandidateTypeSig =
      unapplyMethod.returnType match {
        // Outer type for unapply is always Option
        case TypeRef(_, _, outerArgs) if outerArgs.nonEmpty =>
          outerArgs.head match {
            // If more than one argument and symbol name matches TupleXX then
            // First try matching apply(_:TupleXX[A,B,..Z]) then apply(A,B,..Z)
            // e.g. Tuple3[A,B,C] => List(List(Tuple3[A,B,C]),List(A,B,C))
            case tupleType@TypeRef(_, symbol, innerArgs)
              if innerArgs.nonEmpty &
                 tupleNamePattern.matcher(symbol.fullName.toString).matches =>
              List(List(tupleType), innerArgs)
            // For anything else just search for apply(_:TypeRef)
            // e.g. Map[A,B] => List(List(Map[A,B]))
            case typeRef@TypeRef(_, _, _) =>
              List(List(typeRef))
          }
      }

    val oomApplyMethodAlt =
      getCompanionMethodOrDie(c)(aType,"apply")
        .asTerm
        .alternatives
        .map(_.asMethod)

    // Search for the first apply method whose type sig matches a candidate
    // type sig. Note: using stream here to avoid search past first match
    val optApplyMethod =
      oomCandidateTypeSig.toStream.flatMap { oomTypeSig =>
        oomApplyMethodAlt.find { alt =>
          val oomApplyTypeSig = alt.paramLists.head.map(_.typeSignature)
          oomApplyTypeSig.size == oomTypeSig.size &&
          oomApplyTypeSig.corresponds(oomTypeSig)(_ =:= _)
        } match {
          case Some(applyMethodAlt) => Some((applyMethodAlt,oomTypeSig))
          case None => None
        }
      }.headOption

    val (applyMethod,oomTypeSig) = optApplyMethod getOrElse {
      c.abort(
        c.enclosingPosition, 
        s"No apply methods matching unapply: ${oomCandidateTypeSig.map { typeSig =>
          s"$aType.apply(${typeSig.map(_.toString).mkString(",")})"
        }.mkString("OR")} )"
      )
    }

    applyMethod
      .paramLists
      .head
      .zip(oomTypeSig)
  }



}
