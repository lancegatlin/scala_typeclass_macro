package org.lancegatlin

import test._

case class TestData(value: String)
object TestData {
  implicit val Printable_TestData = Macro.printable[TestData]
}

case class TestData2(value1: Int, value2: String, value3: Option[Double],value4: TestData)
object TestData2 {
  implicit val Printable_TestData = Macro.printable[TestData2]
}

case class TestData3(value1: (Int,String,Double))
object TestData3 {
  implicit val Printable_TestData = Macro.printable[TestData3]
}

case class TestData4(value1: Int, value2: TestData2, value3: List[Double], value4: TestData3)
object TestData4 {
  implicit val Printable_TestData = Macro.printable[TestData4]
}

sealed trait TestEnum
case object TestEnum1 extends TestEnum
case object TestEnum2 extends TestEnum
case object TestEnum3 extends TestEnum
object TestEnum {
  val values = List(TestEnum1,TestEnum2,TestEnum3)
  def apply(value1: String) : TestEnum = values.find(_.toString == value1).get
  def unapply(e:TestEnum) : Option[String] = Some(e.toString)
  implicit val Printable_TestEnum = Macro.printable[TestEnum]
}

sealed trait TestBaseADT
case class TestADT1(value1: String) extends TestBaseADT
object TestADT1 {
  implicit val Printable_TestADT1 = Macro.printable[TestADT1]
}
case class TestADT2(value1: Double) extends TestBaseADT
object TestBaseADT {
  def apply(_type: String, value1: Option[String], value2: Option[Double]) : TestBaseADT = {
    _type match {
      case "TestADT1" => TestADT1(value1.get)
      case "TestADT2" => TestADT2(value2.get)
    }
  }
  def unapply(tb:TestBaseADT) : Option[(String,Option[String],Option[Double])] = {
    tb match {
      case TestADT1(value1) => Some(("TestADT1",Some(value1),None))
      case TestADT2(value2) => Some(("TestADT2",None,Some(value2)))
    }
  }
  implicit val Printable_TestBaseADT = Macro.printable[TestBaseADT]
}

case class TestData5(value1: TestBaseADT, value2: TestEnum)

object TestData5 {
  implicit val Printable_TestData5 =
    Macro.printable[TestData5]
}

case class TestData6[A <: TestBaseADT](value1: A)

object TestData6 {
  implicit def mkPrintable_Testdata6[A <: TestBaseADT](implicit aPrintable:Printable[A]) : Printable[TestData6[A]] =
    Macro.printable[TestData6[A]]
}