package org.familysearch.joetools.simpledb


object RangeSet{
  implicit def apply(range: Range): RangeSet = new RangeSet(range)
}

class RangeSet(private val range: Range) extends Set[Int]{
  override def contains(elem: Int): Boolean = range.contains(elem)

  override def +(elem: Int): Set[Int] = range.toSet.+(elem)

  override def -(elem: Int): Set[Int] = range.toSet.-(elem)

  override def iterator: Iterator[Int] = range.iterator
}
