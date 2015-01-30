package org.familysearch.joetools.simpledb


trait Test {
//  def evaluate(rowIndexEntry: Map[String, AnyRef]): Boolean
  def evaluate(index: UnorderedIndex[_]): Set[Int]
}
