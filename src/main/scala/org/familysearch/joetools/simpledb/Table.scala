package org.familysearch.joetools.simpledb

import scala.collection.SortedMap

abstract class Table[T] {
  def getMatchingRows(matchSpecifier: RowSpecifier): List[T]
  def getMapOfMatchingRows(matchSpecifier: RowSpecifier): scala.collection.immutable.Map[scala.collection.immutable.Map[String, AnyRef], List[T]]

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

  private def getSortedMapOfKeysAndValuesNotSpecifiedInTheQuery(
                                                                 tagsAndValues: scala.collection.immutable.Map[String, AnyRef],
                                                                 tagstoSkip: scala.collection.Set[String], rowIndexEntry: scala.collection.immutable.Map[String, AnyRef]
                                                                 ): SortedMap[String, AnyRef] = {
    var filteredIndexEntry = scala.collection.SortedMap[String, AnyRef]()

    for((tag, value) <- rowIndexEntry){
      if(!tagsAndValues.contains(tag)  && !tagstoSkip.contains(tag)){
        filteredIndexEntry = filteredIndexEntry.+((tag, value))
      } else {
        val tagsAndValuesValue = tagsAndValues.get(tag)
        if(tagsAndValuesValue!=None && tagsAndValuesValue.get != value){
          println("Warning: index entry should have had value (" + tagsAndValuesValue + ") for tag " + tag + " but instead it had value (" + value + ")...")
        }
      }
    }
    filteredIndexEntry
  }

  def getMappedListOfValuesMatchingSpecifierGroupedByConcatinatedUniqueValues(tagsAndValues: scala.collection.immutable.Map[String, AnyRef], tagstoSkip: scala.collection.Set[String], function: Function[T, String], separator: String): scala.collection.immutable.Map[String, _ <: List[String]] = {
    val rowSpecifier = RowSpecifier(tagsAndValues)
    val mappedRows: scala.collection.immutable.Map[scala.collection.immutable.Map[String, AnyRef], List[T]] = getMapOfMatchingRows(rowSpecifier)
    var out = scala.collection.immutable.Map[String, List[String]]()
    for ((rowIndexEntry, valueList) <- mappedRows) {
      val key: String = getSortedMapOfKeysAndValuesNotSpecifiedInTheQuery(tagsAndValues, tagstoSkip, rowIndexEntry).values.mkString(separator)
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


}
