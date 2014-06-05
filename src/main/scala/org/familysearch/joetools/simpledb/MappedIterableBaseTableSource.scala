package org.familysearch.joetools.simpledb

import org.familysearch.joetools.simpledb.SimpleTable.BaseTableSource

class MappedIterableBaseTableSourceIterator[T](private val baseIterator: Iterator[T], companion: Companion[T]) extends Iterator[(Map[String, AnyRef], T)]{
  def hasNext: Boolean = baseIterator.hasNext

  def next(): (Map[String, AnyRef], T) = {
    val nextInstance = baseIterator.next()
    companion.toMap(nextInstance)-> nextInstance
  }
}

class MappedIterableBaseTableSource[T](private val baseIterable: Iterable[T], companion: Companion[T]) extends BaseTableSource[T] {
  def iterator: Iterator[(Map[String, AnyRef], T)] = new MappedIterableBaseTableSourceIterator[T](baseIterable.iterator, companion)
}
