package org.familysearch.joetools.simpledb

class True extends Test {
//  def evaluate(rowIndexEntry: Map[String, AnyRef]): Boolean = { true }
  def evaluate(index: UnorderedIndex[_]): Set[Int] = index.getIndexSet
}