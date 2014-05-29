package org.familysearch.joetools.simpledb

trait Function[T, R] {
  def get(row: T): R
}
