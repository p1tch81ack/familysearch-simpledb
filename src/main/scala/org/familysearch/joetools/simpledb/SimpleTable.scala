package org.familysearch.joetools.simpledb

import org.familysearch.joetools.simpledb.SimpleTable.BaseTableSource
import scala.collection.SortedMap

object SimpleTable {
  type BaseTableSource[T] =  Iterable[Tuple2[Map[String, AnyRef], T]]
}

class SimpleTable[T](protected val tableSource: BaseTableSource[T]) {
  def this(baseIterable: Iterable[T], companion: Companion[T]) = this(new MappedIterableBaseTableSource[T](baseIterable, companion).toMap)
  def this(baseIterable: java.lang.Iterable[T], companion: Companion[T]) = this(new MappedJavaIterableBaseTableSource[T](baseIterable, companion).toMap)

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

  def getMappedListOfValuesMatchingSpecifierGroupedByConcatinatedUniqueValues(tagsAndValues: Map[String, AnyRef], function: Function[T, String], separator: String): Map[String, _ <: List[String]] = {
    val rowSpecifier = RowSpecifier(tagsAndValues)
    val mappedRows: Map[Map[String, AnyRef], List[T]] = getMapOfMatchingRows(rowSpecifier)
    var out = Map[String, List[String]]()
    for ((rowIndexEntry, valueList) <- mappedRows) {
      val key: String = getSortedMapOfKeysAndValuesNotSpecifiedInTheQuery(tagsAndValues, rowIndexEntry).values.mkString(separator)
      var lines = List[String]()
      if(out.contains(key)) {
        lines = out(key)
      }
      for (row <- valueList) {
        val value = function.get(row)
        if (value != null) {
          lines = lines.:: (value.toString)
        }
      }
      out = out + (key -> lines)
    }
    out
  }


  private def getSortedMapOfKeysAndValuesNotSpecifiedInTheQuery(tagsAndValues: Map[String, AnyRef],
                                                       rowIndexEntry: Map[String, AnyRef]): SortedMap[String, AnyRef] = {
    var filteredIndexEntry = scala.collection.SortedMap[String, AnyRef]()

    for((tag, value) <- rowIndexEntry){
      if(!tagsAndValues.contains(tag)){
        filteredIndexEntry = filteredIndexEntry.+((tag, value))
      } else {
        val tagsAndValuesValue = tagsAndValues(tag)
        if(tagsAndValuesValue != value){
          println("Warning: index entry should have had value (" + tagsAndValuesValue + ") for tag " + tag + " but instead it had value (" + value + ")...")
        }
      }
    }
    filteredIndexEntry
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
