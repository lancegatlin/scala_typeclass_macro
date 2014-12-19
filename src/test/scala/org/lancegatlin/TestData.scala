package org.lancegatlin


case class TestData(value: String)
object TestData {
  implicit val Printable_TestData = Macro.printable[TestData]
}

case class TestData2(value1: Int, value2: String, value3: Option[Double],value4: TestData)
object TestData2 {
  implicit val Printable_TestData = Macro.printable[TestData2]
}

case class TestData3(value1: Int, value2: TestData2, value3: List[Double])
object TestData3 {
  implicit val Printable_TestData = Macro.printable[TestData3]
}