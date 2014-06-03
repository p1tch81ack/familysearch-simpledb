package org.familysearch.joetools.simpledb

class MappedIterableBaseTableSourceIterator[T](private val baseIterator: Iterator[T], fieldMap: Map[String, (T)=>_<:AnyRef]) extends Iterator[(Map[String, AnyRef], T)]{
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

class MappedIterableBaseTableSource[T](private val baseIterable: Iterable[T], fieldMap: Map[String, (T)=>_<:AnyRef]) extends BaseTableSource[T] {
  def iterator: Iterator[(Map[String, AnyRef], T)] = new MappedIterableBaseTableSourceIterator[T](baseIterable.iterator, fieldMap)
}
