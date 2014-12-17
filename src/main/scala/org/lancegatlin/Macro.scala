package org.lancegatlin

import scala.language.experimental.macros
import scala.reflect.macros.Context
import scala.annotation.StaticAnnotation
import scala.reflect.runtime.{universe => ru}

object Macro {

  def debugMacro(c: Context)(s: c.Expr[Any]) : c.Expr[Unit] = {
    import c.universe._
    val paramRep = show(s.tree)
    c.Expr(q"""println($paramRep + " = " + $s)""")
  }


  def debug(s: Any) : Unit = macro debugMacro

  def printable[A] : Printable[A] = macro mkPrintable[A]

  def mkPrintable[A: c.WeakTypeTag](c: Context): c.Expr[Printable[A]] = {
//    val h = Helper[A,Opts](c)
//    val body = h.readBody
    c.universe.reify {
      new Printable[A] {
        def print(a: A): String = "here"
      }
    }
  }
}
