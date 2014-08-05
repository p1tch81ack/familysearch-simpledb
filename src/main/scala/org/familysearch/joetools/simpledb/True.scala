package org.familysearch.joetools.simpledb

class True extends Test {
//  def evaluate(rowIndexEntry: Map[String, AnyRef]): Boolean = { true }
  def evaluate(table: SimpleTable[_]): Set[Int] = table.getIndexSet
}