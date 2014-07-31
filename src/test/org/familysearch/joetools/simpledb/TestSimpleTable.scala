package org.familysearch.joetools.simpledb

import junit.framework.TestCase
import junit.framework.Assert.assertEquals

class TestSimpleTable extends TestCase{
  case class Foo(a: java.lang.Integer, b: String)
  object JavaFooCompanion {
    def get(theInstance: Foo, fieldName: String): AnyRef = {
      fieldName match {
        case "a" => theInstance.a
        case "b" => theInstance.b
        case _ => null
      }
    }
//    def fieldNames(theInstance: Foo) = super.toMap(theInstance).keySet
  }

  def testTableCreation() {
    val fooList = List[Foo]()
    /*val table = */ new SimpleTable[Foo](fooList)
  }

  def testTableCreationFromJavaCollection() {
    val fooList = new java.util.LinkedList[Foo]()
    /*val table = */new SimpleTable[Foo](fooList, classOf[Foo])
    val foo=Foo(1, "A")
    /*val fooUnapply: Option[_<:Product] =*/ Foo.unapply(foo)
  }

  def testGetAllRows {
    val foo: Foo = new Foo(1, "A")
    val fooList: List[Foo] = List[Foo]().::(foo)
    val fooTable: SimpleTable[Foo] = new SimpleTable[Foo](fooList)
    val matchingRows: List[Foo] = fooTable.getMatchingRows(new RowSpecifier)
    assertEquals(1, matchingRows.length)
    assertEquals(foo.b, matchingRows.apply(0).b)
    assertEquals(foo.a, matchingRows.apply(0).a)
  }
}
