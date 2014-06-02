package org.familysearch.joetools.simpledb


class SimpleHashMapTable[T](fieldMap: Map[String, (T)=>AnyRef]) extends SimpleTable[T](fieldMap) {
  private val rows = new scala.collection.mutable.HashMap[Map[String, AnyRef], List[T]]

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

  def getMatchingRows(matchSpecifier: RowSpecifier): List[T] = {
    var matchingRows = List[T]()
    for (rowIndexEntry <- rows.keySet) {
      if (matchSpecifier.matches(rowIndexEntry)) {
        for(row <- rows(rowIndexEntry)){
          matchingRows = matchingRows.::( row )
        }
      }
    }
    matchingRows
  }

  def getMapOfMatchingRows(matchSpecifier: RowSpecifier): Map[Map[String, AnyRef], List[T]] = {
    var matchingRowMap: Map[Map[String, AnyRef], List[T]] = Map[Map[String, AnyRef], List[T]]()
    for (rowIndexEntry <- rows.keySet) {
      if ((matchSpecifier eq null) || matchSpecifier.matches(rowIndexEntry)) {
        matchingRowMap = matchingRowMap + (rowIndexEntry -> rows.get(rowIndexEntry).get)
      }
    }
    matchingRowMap
  }


}
