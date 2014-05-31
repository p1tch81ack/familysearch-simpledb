package org.familysearch.joetools.simpledb


class FieldMap[T] {
  def get(instance: T, fieldName: String):AnyRef = null
  def fieldNames: List[String] = fieldNamesArray.toList
  def apply(instance: T): Map[String, AnyRef] = (for(fieldName <- fieldNames) yield (fieldName, get(instance, fieldName))).toMap
  protected def fieldNamesArray: Array[String] = Array[String]()
}
