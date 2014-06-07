package org.familysearch.joetools.simpledb

class HasTagValue(private var tag: String, private var value: AnyRef) extends Test {
  def evaluate(rowIndexEntry: Map[String, AnyRef]): Boolean = {rowIndexEntry.contains(tag) && (value == rowIndexEntry(tag))}
}
