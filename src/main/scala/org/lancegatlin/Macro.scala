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

    val oomArg = oomNormArg(c)(aType)

    val values =
      if(oomArg.size == 1) {
        val (symbol, _type) = oomArg.head
        val innerPrintableType = appliedType(printableTypeConstructor, List(_type))
        val innerPrintable = inferImplicitOrDie(c)(innerPrintableType)
        q"""${symbol.name.toString} + "=" + $innerPrintable.print(tuple)""" :: Nil
      } else {
        oomArg
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
   * Get the "normalized" arguments for a type. The argument types are extracted
   * from the unapply method. The argument names are extracted from the apply
   * method whose argument types match the unapply method.
   * @param c context
   * @param aType type to compute arguments for
   * @return non-empty list of argument (Symbol,Type) pairs
   */
  def oomNormArg(
    c: blackbox.Context
  )(
    aType: c.Type
  ) : List[(c.Symbol, c.Type)] = {
    val (applyMethod, unapplyMethod) = getApplyUnapplyMethodsOrDie(c)(aType)
    val oomType = oomNormType(c)(unapplyMethod)
    val oomApplyMethodArg : List[c.Symbol] = applyMethod.paramLists.head
    oomApplyMethodArg.zip(oomType)
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
   * Get a symbol to the apply method for a type's companion object within a 
   * context. Aborts if apply doesn't exist.
   * @param c context
   * @param aType type whose companion object should be searched for apply 
   *              method
   * @return a method symbol to the apply method
   */
  def getApplyMethodOrDie(
    c: blackbox.Context
  )(
    aType: c.Type
  ) : c.universe.MethodSymbol = {
    import c.universe._
    aType.typeSymbol.companion.typeSignature.decl(TermName("apply")) match {
      case NoSymbol => 
        c.abort(
          c.enclosingPosition,
          s"$aType.apply method does not exist"
        )
      case s => s.asMethod
    }
  }

  /**
   * Get a symbol to the unapply method for a type's companion object within a 
   * context. Aborts if unapply doesn't exist
   * @param c context
   * @param aType type whose companion object should be searched for unapply 
   *              method
   * @return a method symbol to the unapply method
   */
  def getUnapplyMethodOrDie(
    c: blackbox.Context
  )(
    aType: c.Type
  ): c.universe.MethodSymbol= {
    import c.universe._
    aType.typeSymbol.companion.typeSignature.decl(TermName("unapply")) match {
      case NoSymbol => 
        c.abort(
          c.enclosingPosition,
          s"$aType.unapply does not exist"
        )
      case s => s.asMethod
    }
  }

  /**
   * Get the apply/unapply method pair for a type's companion object within a
   * context. The type's companion object apply methods are searched for the 
   * first apply method whose type arguments match the types of the inner types 
   * returned by the unapply method(Option[(inner1,inner2,...)]). Aborts if 
   * either apply or unapply don't exist or if none of the apply methods "match"
   * the unapply method.
   * @param c context
   * @param aType type whose companion object should be searched for apply and
   *              unapply methods
   * @return a pair (apply method, unapply method) where the apply and unapply
   *         methods "match".
   */
  def getApplyUnapplyMethodsOrDie(
    c: blackbox.Context
  )(
    aType: c.Type
  ): (c.universe.MethodSymbol, c.universe.MethodSymbol) = {
    import c.universe._
    val unapplyMethod = getUnapplyMethodOrDie(c)(aType)

    val oomApplyAlt =
      getApplyMethodOrDie(c)(aType).asTerm.alternatives.map(_.asMethod)
    val oomUnapplyType = oomNormType(c)(unapplyMethod)
    val optApplyMethod = oomApplyAlt.find { alt =>
      val oomApplyType = alt.paramLists.head.map(_.typeSignature)
      oomApplyType.size == oomUnapplyType.size &&
      oomApplyType.corresponds(oomUnapplyType)(_ =:= _)
    }

    val applyMethod = optApplyMethod getOrElse {
      c.abort(
        c.enclosingPosition, 
        s"$aType.apply(${oomUnapplyType.map(_.toString).mkString(",")} does not exist)"
      )
    }
    (applyMethod,unapplyMethod)
  }

  val tupleNamePattern = "scala\\.Tuple\\d{1,2}".r.pattern

  /**
   * Get the "normalized" types for a type from the unapply method. The return
   * types of the unapply method are first normalized by discarding the outer
   * Option. Then if the inner type is a scala.Tuple type then the Tuple type is
   * discarded and its type parameters are returned. If the inner type is not a 
   * scala.Tuple type then the inner type is returned unmodified.
   * @param c context
   * @param unapplyMethod unapply method for the type's companion object
   * @return non-empty list of normalized unapply return types
   */
  def oomNormType(
    c: blackbox.Context
  )(
    unapplyMethod: c.universe.MethodSymbol
  ): List[c.Type] = {
    import c.universe._
    unapplyMethod.returnType match {
      // Outer type for unapply is always Option
      case TypeRef(_, _, outerArgs) if outerArgs.nonEmpty =>
        outerArgs.head match {
          // If more than one argument and symbol name matches TupleXX
          // e.g. Tuple3[A,B,C] => List(A,B,C)
          case TypeRef(_, symbol, innerArgs)
            if innerArgs.nonEmpty &
               tupleNamePattern.matcher(symbol.fullName.toString).matches =>
            innerArgs
          // For anything else with inner args, just return the type
          // e.g. Map[A,B] => Map[A,B]
          case typeRef@TypeRef(_, _, _) =>
            List(typeRef)
        }
      case _ => c.abort(c.enclosingPosition, "Unreachable")
    }
  }

}
