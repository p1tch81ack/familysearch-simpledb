package org.familysearch.joetools.simpledb

object RowSpecifier {
  def apply(tagsAndValues: Map[String, AnyRef]): RowSpecifier = {
    var rowSpecifier = new RowSpecifier
    for(tag <- tagsAndValues.keySet){
      rowSpecifier = rowSpecifier.`with`(tag, tagsAndValues(tag))
    }
    rowSpecifier
  }
}

class RowSpecifier(private val test: Test) {

  def this(tag: String, value: AnyRef) {
    this(new HasTagValue(tag, value))
  }


  private def this(parent: RowSpecifier, test: Test) {
    this(new And(parent.test, test))
  }

  private def this(parent: RowSpecifier, tag: String, value: AnyRef) {
    this(parent, new HasTagValue(tag, value))
  }

  def this() {
    this(new True)
  }

  def `with`(tag: String, value: AnyRef): RowSpecifier = {
    if (value != null) {
      new RowSpecifier(this, tag, value)
    }
    else {
      this
    }
  }

  def and(test: Test): RowSpecifier = {
    new RowSpecifier(this, test)
  }

  def andNot(test: Test): RowSpecifier = {
    new RowSpecifier(this, new Not(test))
  }

  def without(tag: String, value: AnyRef): RowSpecifier = {
    andNot(new HasTagValue(tag, value))
  }

  def matches(target: Map[String, AnyRef]): Boolean = {
    test.evaluate(target)
  }
}
