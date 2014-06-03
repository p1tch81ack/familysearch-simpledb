package org.familysearch.joetools.simpledb


class SimpleHashMapTable[T](tableSource: BaseTableSource[T]) extends SimpleTable[T](tableSource) {
//  private val rows = new scala.collection.mutable.HashMap[Map[String, AnyRef], List[T]]
//  private val rows: Iterable[T] = List[T]()


  /*
  def this (fieldMap: FieldMap[T]) = this(SimpleTable.getFieldMap(fieldMap))

  def addRow(row: T) {
    var rowIndexEntry = Map[String, AnyRef]()
    for(fieldName <- fieldMap.keys){
      val value = fieldMap(fieldName)(row)
      if(value!=null){
        rowIndexEntry = rowIndexEntry + (fieldName -> value)
      }
    }
    rowIndexEntry
    var rowList: List[T] = List[T] ()
    if(rows.contains(rowIndexEntry)){
      rowList = rows(rowIndexEntry)
    }
    rows.put(rowIndexEntry, rowList.::(row))
  }
*/
  def getMatchingRows(matchSpecifier: RowSpecifier): List[T] = {
    var matchingRows = List[T]()
    for ((rowIndexEntry, instance) <- tableSource) {
      if (matchSpecifier.matches(rowIndexEntry)) {
        matchingRows ::= instance
      }
    }
    matchingRows
  }

  def getMapOfMatchingRows(matchSpecifier: RowSpecifier): Map[Map[String, AnyRef], List[T]] = {
    var matchingRowMap: Map[Map[String, AnyRef], List[T]] = Map[Map[String, AnyRef], List[T]]()
    for ((rowIndexEntry, instance) <- tableSource) {
      if ((matchSpecifier eq null) || matchSpecifier.matches(rowIndexEntry)) {
        var instanceList = List[T]()
        if(matchingRowMap.contains(rowIndexEntry)){
          instanceList = matchingRowMap(rowIndexEntry)
        }
        instanceList ::= instance
        matchingRowMap += (rowIndexEntry->instanceList)
      }
    }
    matchingRowMap
  }


}
