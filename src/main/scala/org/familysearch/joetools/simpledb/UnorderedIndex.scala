package org.familysearch.joetools.simpledb

abstract class UnorderedIndex[T] {
  def hasTagValue(tag: String, value: AnyRef): Set[Int]
  def getIndexSet: Set[Int]
}
