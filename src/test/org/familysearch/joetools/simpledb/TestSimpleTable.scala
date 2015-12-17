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

  def testGetFieldNames_ObviousFieldMapper() {
    assertFieldNames(createTableWithObviousFieldMapper)
  }

  def testGetFieldNames_ObviousFieldMapper_java() = {
    assertFieldNames(createTableWithObviousFieldMapper_java)
  }

  def testGetSpecifierValues_ObviousFieldMapper(): Unit = {
    val aValues = createTableWithObviousFieldMapper.getSpecifierValues("a")
    assertTrue(aValues.size == 1)
    assertTrue(aValues.head.asInstanceOf[java.lang.Integer] == 1)
    val bValues = createTableWithObviousFieldMapper.getSpecifierValues("b")
    assertTrue(bValues.size == 1)
    assertTrue(bValues.head.asInstanceOf[String] == "A")
  }

  private def assertFieldNames(fooTable: SimpleTable[Foo]) = {
    val fieldNames = fooTable.fieldNames
    assertTrue(fieldNames.contains("a"))
    assertTrue(fieldNames.contains("b"))
    assertTrue(fieldNames.size == 2)
  }

  private def assertAllRows(fooTable: SimpleTable[Foo]) = {
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
