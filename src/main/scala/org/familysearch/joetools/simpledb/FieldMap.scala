package org.familysearch.joetools.simpledb


trait FieldMap[T] {
  def get(instance: T, fieldName: String):AnyRef = null
  def fieldNames: List[String] = List()
}
