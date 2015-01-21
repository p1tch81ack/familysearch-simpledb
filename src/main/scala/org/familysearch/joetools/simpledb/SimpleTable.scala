package org.familysearch.joetools.simpledb

import scala.collection.{mutable, SortedMap}
import scala.collection.JavaConverters._
import scala.reflect.ClassTag

private class TagValueIndexes {
  private val valueIndexes = new mutable.HashMap[AnyRef, mutable.Set[Int]]
  private val nullIndexes= new mutable.HashSet[Int]

  def addEntry(index: Int, value: AnyRef){
    var indexSet: mutable.Set[Int] = null
    if(value eq null){
      nullIndexes.add(index)
    } else {
      if (valueIndexes.contains(value)) {
        indexSet = valueIndexes(value)
      } else {
        indexSet = new mutable.HashSet[Int]()
        valueIndexes.put(value, indexSet)
      }
      indexSet.add(index)
    }
  }

  def hasValue(value: AnyRef): Set[Int] = {
    if(value eq null){
      nullIndexes.toSet
    } else {
      if(valueIndexes.contains(value)){
        valueIndexes(value).toSet
      } else {
        Set[Int]()
      }
    }
  }

  def values: Set[AnyRef] = {
    val ret: Set[AnyRef] = valueIndexes.keySet.toSet
    ret
  }
}

class SimpleTable[T](baseIterable: Iterable[T])(implicit classTag:ClassTag[T] ) {
  private val tableData: Array[T] = baseIterable.toArray[T](classTag)
  protected val tableValues: Iterable[Tuple2[scala.collection.immutable.Map[String, AnyRef], Int]] = {
    (for(i <- 0 until tableData.size)
      yield toMap(tableData(i)) -> i).toMap
  }
  private val tagValueMap: mutable.HashMap[String, TagValueIndexes] = {
    val map = new mutable.HashMap[String, TagValueIndexes]
    val obviousFieldNames = getObviousFieldNames
    for(name <- obviousFieldNames){
      map.put(name, new TagValueIndexes())
    }
    for(i <- 0 until tableData.size){
      val entry = tableData(i)
      for(name <- obviousFieldNames) {
        val mapForTag = map(name)
        val value = getValueOfFieldWithObviousName(entry, name)
        mapForTag.addEntry(i, value)
      }
    }
    map
  }

  def this(baseIterable: java.lang.Iterable[T], clazz: Class[T]) = this(baseIterable.asScala)(ClassTag[T](clazz))

  def hasTagValue(tag: String, value: AnyRef): Set[Int] = {
    if(tagValueMap.contains(tag)){
      val tagValueIndices = tagValueMap(tag)
      tagValueIndices.hasValue(value)
    } else {
      Set[Int]()
    }
  }

  def getIndexSet: Set[Int] = RangeSet(tableData.indices)

  private def getObviousFieldNames: Array[String] = {
    for(f <- classTag.runtimeClass.getDeclaredFields filter (!_.getName.contains('$'))) yield f.getName
  }

  private def getValueOfFieldWithObviousName(instance: T, name: String): AnyRef = {
    val f = classTag.runtimeClass.getDeclaredField(name)
    f.setAccessible(true)
    f.get(instance)
  }

  private def toMap(instance: T): Map[String, AnyRef] = {
    (for(name <- getObviousFieldNames) yield name -> getValueOfFieldWithObviousName(instance, name)).toMap
  }

/*
  def getMatchingRows(matchSpecifier: RowSpecifier): List[T] = {
    var matchingRows = List[T]()
    for ((rowIndexEntry, index) <- tableValues) {
      if (matchSpecifier.matches(rowIndexEntry)) {
        matchingRows ::= tableData(index)
      }
    }
    matchingRows
  }

  def getMapOfMatchingRows(matchSpecifier: RowSpecifier): scala.collection.immutable.Map[scala.collection.immutable.Map[String, AnyRef], List[T]] = {
    var matchingRowMap = scala.collection.immutable.Map[scala.collection.immutable.Map[String, AnyRef], List[T]]()
    for ((rowIndexEntry, index) <- tableValues) {
      if ((matchSpecifier eq null) || matchSpecifier.matches(rowIndexEntry)) {
        var instanceList = List[T]()
        if(matchingRowMap.contains(rowIndexEntry)){
          instanceList = matchingRowMap(rowIndexEntry)
        }
        instanceList ::= tableData(index)
        matchingRowMap += (rowIndexEntry->instanceList)
      }
    }
    matchingRowMap
  }
*/

  def getMatchingRows(matchSpecifier: RowSpecifier): List[T] = {
    val matchingIndices = matchSpecifier.matches(this)
    var matchingRows = List[T]()
    for (index<-matchingIndices) {
        matchingRows ::= tableData(index)
    }
    matchingRows
  }

  def getMapOfMatchingRows(matchSpecifier: RowSpecifier): scala.collection.immutable.Map[scala.collection.immutable.Map[String, AnyRef], List[T]] = {
    val matchingIndices = matchSpecifier.matches(this)
    var matchingRowMap = scala.collection.immutable.Map[scala.collection.immutable.Map[String, AnyRef], List[T]]()
    for (index <- matchingIndices) {
      val rowIndexEntry = toMap(tableData(index))
        var instanceList = List[T]()
        if(matchingRowMap.contains(rowIndexEntry)){
          instanceList = matchingRowMap(rowIndexEntry)
        }
        instanceList ::= tableData(index)
        matchingRowMap += (rowIndexEntry->instanceList)
    }
    matchingRowMap
  }



  def getSpecifierValues(specifierTag: String): Set[AnyRef] = {
    val tableEntryForTag: TagValueIndexes = tagValueMap(specifierTag)
    tableEntryForTag.values
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
