package org.familysearch.joetools.simpledb

import scala.collection.immutable.TreeSet
import scala.collection

object SimpleTable {
  def getFieldMap[T](fieldMapInstance: FieldMap[T]): Map[String, (T)=>AnyRef] = {
    (for(fieldName <-fieldMapInstance.fieldNames) yield (fieldName, (instance:T)=>{fieldMapInstance.get(instance, fieldName)})).toMap
  }

}

abstract class SimpleTable[T](protected val fieldMap: Map[String, (T)=>AnyRef]) {

  def getMatchingRows(matchSpecifier: RowSpecifier): List[T]

  def getMapOfMatchingRows(matchSpecifier: RowSpecifier): Map[Map[String, AnyRef], List[T]]

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
