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

