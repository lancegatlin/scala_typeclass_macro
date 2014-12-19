package org

package object lancegatlin {
  implicit object Printable_Int extends SimplePrintable[Int]
  implicit object Printable_Double extends SimplePrintable[Double]
  implicit val Printable_String = new Printable[String] {
    override def print(a: String): String = s""""$a""""
  }
  implicit def mkPrintable_Option[A](implicit pA:Printable[A]) = new Printable[Option[A]] {
    override def print(oa: Option[A]): String = oa match {
      case Some(a) => s"Some(${pA.print(a)})"
      case None => "None"
    }
  }
  implicit def mkPrintable_List[A](implicit pA:Printable[A]) = new Printable[List[A]] {
    override def print(oa: List[A]): String = oa match {
      case Nil => "Nil"
      case nonEmptyList => s"List(${nonEmptyList.map(a => pA.print(a)).mkString(",")})"
    }
  }
  implicit class PimpEverything[A](val self: A) extends AnyVal {
    def print(implicit p:Printable[A]) : String = p.print(self)
  }
}