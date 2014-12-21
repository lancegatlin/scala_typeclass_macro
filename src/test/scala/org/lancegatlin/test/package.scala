package org.lancegatlin

package object test {
  implicit def mkPrintable_Tuple3[A,B,C](implicit
    aPrintable:Printable[A],
    bPrintable:Printable[B],
    cPrintable:Printable[C]
  ) = Macro.printable[(A,B,C)]
}
