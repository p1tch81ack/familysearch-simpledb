package org.familysearch.joetools.simpledb

import scala.reflect.ClassTag

class Companion[T](implicit val classTag: ClassTag[T]) {
//  def this(clazz: Class[T]) = {this(ClassTag[T].apply(clazz.))}
  private def getDeclaredFields = classTag.runtimeClass.getDeclaredFields
  def toMap(instance: T): Map[String, AnyRef] = {
    var ret = Map[String, AnyRef]()
    for(f<-getDeclaredFields){
      f.setAccessible(true)
      if(!f.getName.contains('$')) {
        ret = ret.+(f.getName -> f.get(instance))
      }
    }
    ret
  }
}
