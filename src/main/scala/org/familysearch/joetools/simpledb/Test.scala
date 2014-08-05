package org.familysearch.joetools.simpledb


trait Test {
//  def evaluate(rowIndexEntry: Map[String, AnyRef]): Boolean
  def evaluate(table: SimpleTable[_]): Set[Int]
}
