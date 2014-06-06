package org.familysearch.joetools.simpledb

import scala.reflect.Manifest

class Companion[T](clazz: Class[_]) {
  def this()(implicit m: Manifest[T])=this(m.erasure)
  private def getDeclaredFields = clazz.getDeclaredFields
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
