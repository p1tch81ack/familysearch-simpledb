package org.familysearch.joetools.simpledb

import org.familysearch.joetools.simpledb.SimpleTable.BaseTableSource

class MappedJavaIterableBaseTableSourceIterator[T](private val baseIterator: java.util.Iterator[T], companion: Companion[T]) extends Iterator[(Map[String, AnyRef], T)]{
  def hasNext: Boolean = baseIterator.hasNext

  def next(): (Map[String, AnyRef], T) = {
    val nextInstance = baseIterator.next()
    companion.toMap(nextInstance) -> nextInstance
  }
}

class MappedJavaIterableBaseTableSource[T](private val baseIterable: java.lang.Iterable[T], companion: Companion[T]) extends BaseTableSource[T] {
  def iterator: Iterator[(Map[String, AnyRef], T)] = new MappedJavaIterableBaseTableSourceIterator[T](baseIterable.iterator, companion)
}
