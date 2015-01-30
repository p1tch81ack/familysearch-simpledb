package org.familysearch.joetools.simpledb

import scala.reflect.ClassTag

object ObviousFieldMapper{
  def apply[T](implicit classTag:ClassTag[T]):ObviousFieldMapper[T]=new ObviousFieldMapper[T]
}

class ObviousFieldMapper[T](implicit classTag:ClassTag[T]) extends FieldMapper[T]{
  val names = (for(f <- classTag.runtimeClass.getDeclaredFields filter (!_.getName.contains('$'))) yield f.getName).toSet

  private def getValueOfFieldWithObviousName(instance: T, name: String): AnyRef = {
    val f = classTag.runtimeClass.getDeclaredField(name)
    f.setAccessible(true)
    f.get(instance)
  }

  override def map(instance: T): Map[String, AnyRef] = {
    (for(fieldName:String <- names) yield (fieldName, getValueOfFieldWithObviousName(instance, fieldName))).toMap
  }
}
