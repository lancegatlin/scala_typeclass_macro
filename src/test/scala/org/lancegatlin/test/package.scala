package org.lancegatlin

package object test {
  implicit def mkPrintable_Tuple3[A,B,C](implicit
    aPrintable:Printable[A],
    bPrintable:Printable[B],
    cPrintable:Printable[C]
  ) = Macro.printable[(A,B,C)]
//  new Printable[(A,B,C)] {
//    override def print(a: (A,B,C)): String = "here"
//  }
}
