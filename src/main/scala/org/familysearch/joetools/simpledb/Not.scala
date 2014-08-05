package org.familysearch.joetools.simpledb

class Not(private val test: Test) extends Test {
//  def evaluate(rowIndexEntry: Map[String, AnyRef]): Boolean = !test.evaluate(rowIndexEntry)
  def evaluate(table: SimpleTable[_]): Set[Int] = {
    table.getIndexSet.diff(test.evaluate(table))
  }
}
