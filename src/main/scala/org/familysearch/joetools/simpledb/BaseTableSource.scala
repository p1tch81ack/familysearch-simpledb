package org.familysearch.joetools.simpledb

object BaseTableSource {
  def getFieldMap[T](fieldMapInstance: FieldMap[T]): Map[String, (T)=>AnyRef] = {
    (for(fieldName <-fieldMapInstance.fieldNames) yield (fieldName, (instance:T)=>{fieldMapInstance.get(instance, fieldName)})).toMap
  }

}

trait BaseTableSource[T] extends Iterable[Tuple2[Map[String, AnyRef], T]]{

}
