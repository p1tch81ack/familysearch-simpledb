package org.familysearch.joetools.simpledb

import org.familysearch.joetools.simpledb.SimpleTable.BaseTableSource

class MappedJavaIterableBaseTableSourceIterator[T](private val baseIterator: java.util.Iterator[T], fieldMap: Map[String, (T)=>AnyRef]) extends Iterator[(Map[String, AnyRef], T)]{
  def hasNext: Boolean = baseIterator.hasNext

  def next(): (Map[String, AnyRef], T) = {
    val nextInstance = baseIterator.next()
    var rowIndexEntry = Map[String, AnyRef]()
    for(fieldName <- fieldMap.keys){
      val value = fieldMap(fieldName)(nextInstance)
      if(value!=null){
        rowIndexEntry = rowIndexEntry + (fieldName -> value)
      }
    }
    rowIndexEntry -> nextInstance
  }
}

class MappedJavaIterableBaseTableSource[T](private val baseIterable: java.lang.Iterable[T], fieldMap: Map[String, (T)=>AnyRef]) extends BaseTableSource[T] {
  def iterator: Iterator[(Map[String, AnyRef], T)] = new MappedJavaIterableBaseTableSourceIterator[T](baseIterable.iterator, fieldMap)
}
