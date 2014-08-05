package org.familysearch.joetools.simpledb

import junit.framework.TestCase

class TestHasTagValue extends TestCase {
  class Foo(val a: String) {}

  def testCreation(){
    val fooList = List[Foo](new Foo("foo"), new Foo("foo"))
    val table = new SimpleTable(fooList)
    val hasTagValue = new HasTagValue("a", "foo")
    val result = hasTagValue.evaluate(table)
    assert(result.contains(0))
  }
}
