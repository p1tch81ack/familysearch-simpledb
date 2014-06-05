package org.familysearch.joetools.simpledb

import java.lang.reflect.Field
import scala.reflect.Manifest

abstract class Companion[T](clazz: Class[_]) {
  def this()(implicit m: Manifest[T])=this(m.erasure)
  private def getDeclaredFields = clazz.getDeclaredFields
  def fieldAndInstanceToFieldNameAndValueMap(instance: T): (Field)=>(String, AnyRef) = {(f: Field) => {f.setAccessible(true); (f.getName, f.get(instance))}}
  def toMap(instance: T): Map[String, AnyRef] = (getDeclaredFields map fieldAndInstanceToFieldNameAndValueMap(instance)).toMap
  /*
  protected def fieldNamesArray: Array[String] = Array[String]()
  protected def valueArray: Array[AnyRef] = Array[AnyRef]()
  */
}
