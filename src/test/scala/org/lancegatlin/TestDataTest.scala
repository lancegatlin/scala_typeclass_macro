package org.lancegatlin

import org.scalatest.{Matchers, FlatSpec}

class TestDataTest extends FlatSpec with Matchers {
  "Macro.printable" must "correctly implement a single value case class" in {
    TestData(value="asdf").print should equal(
      """TestData(value="asdf")"""
    )
  }
  "Macro.printable" must "correctly implement a case class with a nested case class value" in {
    TestData2(value1=1,value2="2",value3=Some(3.0),value4=TestData(value="asdf")).print should equal(
      """TestData2(value1=1,value2="2",value3=Some(3.0),value4=TestData(value="asdf"))"""
    )
  }
  "Macro.printable" must "correctly implement a case class with a single tuple value" in {
    TestData3(value1=(1,"2",3.0)).print should equal(
      """TestData3(value1=(1,"2",3.0))"""
    )
  }
  "Macro.printable" must "correctly implement a case class with a collection" in {
    TestData4(value1=2,value2=TestData2(value1=1,value2="2",value3=Some(10.2),value4=TestData(value="asdf")),value3=List(1.1,2.2,3.3),value4=TestData3(value1=(2,"3",4.0))).print should equal(
      """TestData4(value1=2,value2=TestData2(value1=1,value2="2",value3=Some(10.2),value4=TestData(value="asdf")),value3=List(1.1,2.2,3.3),value4=TestData3(value1=(2,"3",4.0)))"""
    )
  }
  "Macro.printable" must "correctly implement a case class with an ADT and Enum" in {
    TestData5(TestADT1("2"),TestEnum2).print should equal(
      """TestData5(value1=TestBaseADT(_type="TestADT1",value1=Some("2"),value2=None),value2=TestEnum(value1="TestEnum2"))"""
    )
  }
  "Macro.printable" must "correctly implement a case class with a type parameter" in {
    TestData6(value1=TestADT1(value1="2")).print should equal(
      """TestData6(value1=TestADT1(value1="2"))"""
    )
  }

}
