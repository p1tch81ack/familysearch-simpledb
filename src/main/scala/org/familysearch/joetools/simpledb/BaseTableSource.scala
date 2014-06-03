package org.familysearch.joetools.simpledb


trait BaseTableSource[T] extends Iterable[Tuple2[Map[String, AnyRef], T]]{

}
