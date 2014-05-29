package org.familysearch.joetools.simpledb

object SimpleTable {
  def getRowIndexEntry[T](instance: T, fieldMap: FieldMap[T]): RowIndexEntry = {
    var rowIndexEntry = new RowIndexEntry
    for(fieldName <- fieldMap.fieldNames){
       rowIndexEntry = rowIndexEntry.add(fieldName, fieldMap.get(instance, fieldName))
    }
    rowIndexEntry
  }

  def getRowIndexEntry[T](instance: T, fieldMap: Map[String, (T)=>AnyRef]): RowIndexEntry = {
    var rowIndexEntry = new RowIndexEntry
    for(fieldName <- fieldMap.keys){
      rowIndexEntry = rowIndexEntry.add(fieldName, fieldMap(fieldName)(instance))
    }
    rowIndexEntry
  }

  def getFieldMap[T](fieldMapInstance: FieldMap[T]): Map[String, (T)=>AnyRef] = {
    (for(fieldName <-fieldMapInstance.fieldNames) yield (fieldName, (instance:T)=>{fieldMapInstance.get(instance, fieldName)})).toMap
  }

}

abstract class SimpleTable[T](protected val fieldMap: Map[String, (T)=>AnyRef]) {

  def getMatchingRows(matchSpecifier: RowSpecifier): java.util.List[T]

  def getMapOfMatchingRows(matchSpecifier: RowSpecifier): java.util.Map[RowIndexEntry, java.util.List[T]]

  def getSpecifierValuesForMatchingRows(rowSpecifier: RowSpecifier, specifierTag: String): java.util.Set[AnyRef] = {
    val specifierValues: java.util.Set[AnyRef] = new java.util.TreeSet[AnyRef]
    import scala.collection.JavaConversions._
    for (rowIndexEntry <- getMapOfMatchingRows(rowSpecifier).keySet) {
      val specifierValue: AnyRef = rowIndexEntry.getSpecifierValue(specifierTag)
      if (!(specifierValue eq null)) {
        specifierValues.add(specifierValue)
      }
    }
    specifierValues
  }

  def getMappedListOfValuesMatchingSpecifierGrupedByConcatinatedUniqueValues(rowSpecifier: RowSpecifier, function: Function[T, AnyRef], separator: String): java.util.Map[String, _ <: java.util.List[String]] = {
    val out: java.util.Map[String, java.util.LinkedList[String]] = new java.util.TreeMap[String, java.util.LinkedList[String]]
    val mappedRows: java.util.Map[RowIndexEntry, java.util.List[T]] = getMapOfMatchingRows(rowSpecifier)
    import scala.collection.JavaConversions._
    for (rowIndexEntry <- mappedRows.keySet) {
      val key: String = concatinateUnspecifiedRowSpecifierValues(rowSpecifier, rowIndexEntry, separator)
      import scala.collection.JavaConversions._
      for (row <- mappedRows.get(rowIndexEntry)) {
        val value: AnyRef = function.get(row)
        if (value != null) {
          var lines: java.util.LinkedList[String] = out.get(key)
          if (lines == null) {
            lines = new java.util.LinkedList[String]
            out.put(key, lines)
          }
          lines.add(value.toString)
        }
      }
    }
    out
  }

  private def concatinateUnspecifiedRowSpecifierValues(rowSpecifier: RowSpecifier, rowIndexEntry: RowIndexEntry, separator: String): String = {
    var key: String = ""
    import scala.collection.JavaConversions._
    for (tag <- rowIndexEntry.getSpecifierTags) {
      if (rowSpecifier.matches(rowIndexEntry.remove(tag))) {
        if (key.length > 0) {
          key += separator
        }
        key += rowIndexEntry.getSpecifierValue(tag)
      }
    }
    key
  }

  def getAverageValueForMatchingRows(rowSpecifier: RowSpecifier, function: Function[T, java.lang.Double]): Double = {
    var total: Double = 0.0
    var count: Int = 0
    import scala.collection.JavaConversions._
    for (row <- getMatchingRows(rowSpecifier)) {
      val item: java.lang.Double = function.get(row)
      if (!(item eq null)) {
        total += item.doubleValue()
        count += 1
      }
    }
    if (count > 0) {
      total / count.asInstanceOf[Double]
    }
    else {
      0.0
    }
  }

}
