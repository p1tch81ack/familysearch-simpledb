package org.familysearch.joetools.simpledb

import scala.collection
import org.familysearch.joetools.simpledb.SimpleTable.BaseTableSource

object SimpleTable {
  type BaseTableSource[T] =  Iterable[Tuple2[Map[String, AnyRef], T]]

  def getFieldMap[T](fieldMapInstance: FieldMap[T]): Map[String, (T)=>AnyRef] = {
    (for(fieldName <-fieldMapInstance.fieldNames) yield (fieldName, (instance:T)=>{fieldMapInstance.get(instance, fieldName)})).toMap
  }
}

class SimpleTable[T](protected val tableSource: BaseTableSource[T]) {

  def this(baseIterable: Iterable[T], fieldMap: Map[String, (T)=>_<:AnyRef]) = this(new MappedIterableBaseTableSource[T](baseIterable, fieldMap).toMap)
  def this(baseIterable: java.lang.Iterable[T], fieldMap: Map[String, (T)=>_<:AnyRef]) = this( new MappedJavaIterableBaseTableSource[T](baseIterable, fieldMap).toMap)
  def this(baseIterable: java.lang.Iterable[T], fieldMap: FieldMap[T]) = this(new MappedJavaIterableBaseTableSource[T](baseIterable, SimpleTable.getFieldMap(fieldMap)).toMap)

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
    var matchingRowMap = Map[Map[String, AnyRef], List[T]]()
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



  def getSpecifierValuesForMatchingRows(rowSpecifier: RowSpecifier, specifierTag: String): Set[AnyRef] = {
    var specifierValues = Set[AnyRef]()
    for (rowIndexEntry <- getMapOfMatchingRows(rowSpecifier).keySet) {
      if(rowIndexEntry.contains(specifierTag)){
        val specifierValue = rowIndexEntry(specifierTag)
        specifierValues += specifierValue
      }
    }
    specifierValues
  }

  def getMappedListOfValuesMatchingSpecifierGrupedByConcatinatedUniqueValues(rowSpecifier: RowSpecifier, function: Function[T, AnyRef], separator: String): Map[String, _ <: List[String]] = {
    var out = Map[String, List[String]]()
    val mappedRows = getMapOfMatchingRows(rowSpecifier)
    for (rowIndexEntry <- mappedRows.keySet) {
      val key: String = concatinateUnspecifiedRowSpecifierValues(rowSpecifier, rowIndexEntry, separator)
      for (row <- mappedRows(rowIndexEntry)) {
        val value: AnyRef = function.get(row)
        if (value != null) {
          var lines = List[String]()
          if(out.contains(key)) {
            lines = out(key)
          }
          lines = lines.:: (value.toString)
          out = out + (key -> lines)
        }
      }
    }
    out
  }


  private def concatinateUnspecifiedRowSpecifierValues(rowSpecifier: RowSpecifier,
                                                       rowIndexEntry: Map[String, AnyRef],
                                                       separator: String): String = {
    var out: String = ""
    val keySet: collection.Set[String] = rowIndexEntry.keySet
    var sortedKeyset = new scala.collection.immutable.TreeSet[String]
    for(key<-keySet){
      sortedKeyset += key
    }
    for (tag <- sortedKeyset) {
        val indexEntryWithTagRemoved = rowIndexEntry - tag
        if (rowSpecifier.matches(indexEntryWithTagRemoved)) {
          if (out.length > 0) {
            out = out + separator
          }
          out = out + rowIndexEntry(tag)
        }
      }
      out
    }

  def getAverageValueForMatchingRows(rowSpecifier: RowSpecifier, function: Function[T, java.lang.Double]): Double = {
    var total: Double = 0.0
    var count: Int = 0
    for (row <- getMatchingRows(rowSpecifier)) {
      val item = function.get(row)
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
