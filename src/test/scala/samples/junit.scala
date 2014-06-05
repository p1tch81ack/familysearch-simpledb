package samples

import org.junit.Test

import org.familysearch.joetools.simpledb.{SimpleTable, Companion}



@Test
class TestSimpleTable {
  object Foo extends Companion[Foo]
  case class Foo(a: java.lang.Integer, b: String)
  object JavaFooCompanion extends Companion[Foo] {
    def get(theInstance: Foo, fieldName: String): AnyRef = {
      fieldName match {
        case "a" => theInstance.a
        case "b" => theInstance.b
        case _ => null
      }
    }
    def fieldNames(theInstance: Foo) = super.toMap(theInstance).keySet
  }

  @Test
  def testTableCreation() {
    val fooList = List[Foo]()
    val table = new SimpleTable[Foo](fooList, Foo)
  }

  @Test
  def testTableCreationFromJavaCollection() {
    val fooList = new java.util.LinkedList[Foo]()
    val table = new SimpleTable[Foo](fooList, JavaFooCompanion)
    val foo=Foo(1, "A")
    val fooUnapply: Option[_<:Product] = Foo.unapply(foo)
  }

}

@Test
class TestCompanion {
  case class Foo(a: java.lang.Integer, b: String)

  object Foo extends Companion[Foo]
  /*
  @Test def testWillFail() {
    assert(false)
  }
  */

  @Test
  def testUnapply() {
    val foo = Foo(1, "A")
    val unapplyFoo = Foo.unapply(foo)
    assert(unapplyFoo.isDefined)
    assert(unapplyFoo.get._1 == 1)
    assert(unapplyFoo.get._2 == "A")
  }

  @Test
  def testProductIterator() {
    val foo = Foo(1, "A")
    val unapplyFoo = Foo.unapply(foo)
    assert(unapplyFoo.isDefined)
    assert(unapplyFoo.get._1 == 1)
    assert(unapplyFoo.get._2 == "A")
  }

  @Test
  def testToMap() {
    val foo = Foo(1, "A")
    val map = Foo.toMap(foo)
    assert(map("a")==1)
    assert(map("b")=="A")
  }
}
