package org.familysearch.joetools.simpledb

import scala.collection.JavaConverters._
import scala.reflect.ClassTag

class SimpleTable[T](baseIterable: Iterable[T], fieldMapper:FieldMapper[T])(implicit classTag:ClassTag[T] ) extends Table[T]{
  private val tableData = baseIterable.toArray[T](classTag).toList
  private val mainIndex = new UnorderedIndexToOrderedDataset[T](tableData, fieldMapper)

  def this(baseIterable: Iterable[T])(implicit classTag:ClassTag[T] ) = this(baseIterable, ObviousFieldMapper(classTag))(classTag)

  def this(baseIterable: java.lang.Iterable[T], clazz: Class[T]) = this(baseIterable.asScala)(ClassTag[T](clazz))

  def getMapOfMatchingRows(matchSpecifier: RowSpecifier): scala.collection.immutable.Map[scala.collection.immutable.Map[String, AnyRef], List[T]] = mainIndex.getMapOfMatchingRows(matchSpecifier)

  def getMatchingRows(matchSpecifier: RowSpecifier): List[T] = mainIndex.getMatchingRows(matchSpecifier)
}


