package org.familysearch.joetools.simpledb

import java.util

import junit.framework.TestCase
import org.junit.Assert._

class TestSimpleTable extends TestCase{
  case class Foo(a: java.lang.Integer, b: String)

  def testTableCreation() {
    val fooList = List[Foo]()
    new SimpleTable[Foo](fooList)
  }

  def testGetAllRows_ObviousFieldMapper() {
    assertAllRows(createTableWithObviousFieldMapper)
  }

  def testGetAllRows_ObviousFieldMapper_java() {
    assertAllRows(createTableWithObviousFieldMapper_java)
  }

  def testGetFieldNames_ObviousFieldMapper(): Unit = {
    assertFieldNames(createTableWithObviousFieldMapper)
  }

  def testGetFieldNames_ObviousFieldMapper_java(): Unit = {
    assertFieldNames(createTableWithObviousFieldMapper_java)
  }

  private def assertFieldNames(fooTable: SimpleTable[Foo]): Unit = {
    val fieldNames = fooTable.getFieldNames()
    assertTrue(fieldNames.contains("a"))
    assertTrue(fieldNames.contains("b"))
    assertTrue(fieldNames.size == 2)
  }

  private def assertAllRows(fooTable: SimpleTable[Foo]): Unit = {
    val matchingRows: List[Foo] = fooTable.getMatchingRows(new RowSpecifier)
    assertEquals(1, matchingRows.length)
    assertEquals("A", matchingRows.head.b)
    assertEquals(1, matchingRows.head.a)
  }

  private def createTableWithObviousFieldMapper: SimpleTable[Foo] = {
    new SimpleTable[Foo](createFooList)
  }

  def createTableWithObviousFieldMapper_java: SimpleTable[Foo] = {
    new SimpleTable[Foo](createFooList_java, classOf[Foo])
  }

  private def createFooList: List[Foo] = {
    List[Foo]().::(new Foo(1, "A"))
  }

  private def createFooList_java: util.LinkedList[Foo] = {
    val fooList = new util.LinkedList[Foo]()
    fooList.add(Foo(1, "A"))
    fooList
  }
}
