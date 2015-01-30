package org.familysearch.joetools.simpledb

import scala.collection.{LinearSeq, mutable}
import scala.reflect.ClassTag

private class TagValueIndexes {
  private val valueIndexes = new mutable.HashMap[AnyRef, mutable.Set[Int]]
  private val nullIndexes= new mutable.HashSet[Int]

  def addEntry[T<:AnyRef](index: Int, value: T){
    var indexSet: mutable.Set[Int] = null
    if(value==null){
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

class UnorderedIndexToOrderedDataset[T](tableData:LinearSeq[T], val fieldMapper:FieldMapper[T])(implicit classTag:ClassTag[T] ) extends UnorderedIndex[T]{

  protected val tableValues: Iterable[Tuple2[scala.collection.immutable.Map[String, AnyRef], Int]] = {
    (for(i <- 0 until tableData.size)
    yield fieldMapper.map(tableData(i)) -> i).toMap
  }

  private val tagValueMap: mutable.HashMap[String, TagValueIndexes] = {
    val map = new mutable.HashMap[String, TagValueIndexes]
    for(fieldName<-fieldMapper.names) map.put(fieldName, new TagValueIndexes)
    for(i <- 0 until tableData.size){
      val entry = tableData(i)
      val fieldMap = fieldMapper.map(entry)
      for(name <- fieldMapper.names) {
        var mapForTag = map(name)
        val value = fieldMap(name)
        mapForTag.addEntry(i, value)
      }
    }
    map
  }

  def this(tableData: LinearSeq[T])(implicit classTag:ClassTag[T] ) = this(tableData, ObviousFieldMapper(classTag))(classTag)

  def hasTagValue(tag: String, value: AnyRef): Set[Int] = {
    if(tagValueMap.contains(tag)){
      val tagValueIndices = tagValueMap(tag)
      tagValueIndices.hasValue(value)
    } else {
      Set[Int]()
    }
  }

  def getIndexSet: Set[Int] = RangeSet(tableData.indices)

  def getMatchingRows(matchSpecifier: RowSpecifier): List[T] = {
    val matchingIndices = matchSpecifier.matches(this)
    var matchingRows = List[T]()
    for (index<-matchingIndices) {
      matchingRows ::= tableData(index)
    }
    matchingRows
  }


  def getSpecifierValues(specifierTag: String): Set[AnyRef] = {
    val tableEntryForTag: TagValueIndexes = tagValueMap(specifierTag)
    tableEntryForTag.values
  }

  def getMapOfMatchingRows(matchSpecifier: RowSpecifier): scala.collection.immutable.Map[scala.collection.immutable.Map[String, AnyRef], List[T]] = {
    val matchingIndices = matchSpecifier.matches(this)
    var matchingRowMap = scala.collection.immutable.Map[scala.collection.immutable.Map[String, AnyRef], List[T]]()

    for (index <- matchingIndices) {
      val rowIndexEntry = fieldMapper.map(tableData(index))
      var instanceList = List[T]()
      if(matchingRowMap.contains(rowIndexEntry)){
        instanceList = matchingRowMap(rowIndexEntry)
      }
      instanceList ::= tableData(index)
      matchingRowMap += (rowIndexEntry->instanceList)
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


}
