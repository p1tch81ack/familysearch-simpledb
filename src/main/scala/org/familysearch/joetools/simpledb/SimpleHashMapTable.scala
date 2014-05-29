package org.familysearch.joetools.simpledb


class SimpleHashMapTable[T/*<:SimpleRow*/](fieldMap: Map[String, (T)=>AnyRef]) extends SimpleTable[T](fieldMap) {
  private val rows = new java.util.HashMap[RowIndexEntry, java.util.List[T]]

  def this () = this(null)

  def addRow(row: T) {
    val indexEntry: RowIndexEntry = SimpleTable.getRowIndexEntry(row, fieldMap)
    var rowList: java.util.List[T] = rows.get(indexEntry)
    if (rowList == null) {
      rowList = new java.util.LinkedList[T]
      rows.put(indexEntry, rowList)
    }
    rowList.add(row)
  }

  def getMatchingRows(matchSpecifier: RowSpecifier): java.util.List[T] = {
    val matchingRows: java.util.List[T] = new java.util.LinkedList[T]
    import scala.collection.JavaConversions._
    for (rowIndexEntry <- rows.keySet) {
      if (matchSpecifier.matches(rowIndexEntry)) {
        matchingRows.addAll(rows.get(rowIndexEntry))
      }
    }
    matchingRows
  }

  def getMapOfMatchingRows(matchSpecifier: RowSpecifier): java.util.Map[RowIndexEntry, java.util.List[T]] = {
    val matchingRowMap: java.util.Map[RowIndexEntry, java.util.List[T]] = new java.util.HashMap[RowIndexEntry, java.util.List[T]]
    import scala.collection.JavaConversions._
    for (rowIndexEntry <- rows.keySet) {
      if ((matchSpecifier eq null) || matchSpecifier.matches(rowIndexEntry)) {
        matchingRowMap.put(rowIndexEntry, rows.get(rowIndexEntry))
      }
    }
    matchingRowMap
  }


}
