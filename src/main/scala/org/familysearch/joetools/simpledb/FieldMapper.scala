package org.familysearch.joetools.simpledb

abstract class FieldMapper[T] {
  def names: Set[String]
  def map(instance: T): Map[String, AnyRef]
}
