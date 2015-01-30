package org.familysearch.joetools.simpledb

class And(private var left: Test, private var right: Test) extends Test {
//  def evaluate(rowIndexEntry: Map[String, AnyRef]): Boolean = left.evaluate(rowIndexEntry) && right.evaluate(rowIndexEntry)
  def evaluate(index: UnorderedIndex[_]): Set[Int] = {
    left.evaluate(index).intersect(right.evaluate(index))
  }
}
