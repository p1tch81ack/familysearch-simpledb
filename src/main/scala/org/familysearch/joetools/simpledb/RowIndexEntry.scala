package org.familysearch.joetools.simpledb

class RowIndexEntry {
  private val tagsAndValues = new java.util.TreeMap[String, AnyRef]

  private def this(parent: RowIndexEntry, tag: String, value: AnyRef) {
    this()
    tagsAndValues.putAll(parent.tagsAndValues)
    if (tagsAndValues.get(tag) != null) {
      throw new IllegalArgumentException("" + tag + " is already set to " + tagsAndValues.get(tag) + " when trying to set it to " + value)
    }
    if (value != null) {
      tagsAndValues.put(tag, value)
    }
  }

  def this(tag: String, value: AnyRef) {
    this(new RowIndexEntry, tag, value)
  }

  private def this(parent: RowIndexEntry, tag: String) {
    this()
    tagsAndValues.putAll(parent.tagsAndValues)
    tagsAndValues.remove(tag)
  }

  def add(tag: String, value: AnyRef): RowIndexEntry = {
    if (value != null) {
      new RowIndexEntry(this, tag, value)
    }
    else {
      this
    }
  }

  def remove(tag: String): RowIndexEntry = {
    new RowIndexEntry(this, tag)
  }

  def getSpecifierValue(tag: String): AnyRef = {
    tagsAndValues.get(tag)
  }

  def matches(target: RowIndexEntry): Boolean = {
    import scala.collection.JavaConversions._
    for (key <- tagsAndValues.keySet) {
      if (!(tagsAndValues.get(key) == target.tagsAndValues.get(key))) {
        return false
      }
    }
    true
  }

  def getSpecifierTags: java.util.Set[String] = {
    tagsAndValues.keySet
  }

}
