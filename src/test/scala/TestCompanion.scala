import org.junit.Test

import org.familysearch.joetools.simpledb.Companion


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
    assert(unapplyFoo.get._1 == new java.lang.Integer(1))
    assert(unapplyFoo.get._2 == "A")
  }

  @Test
  def testProductIterator() {
    val foo = Foo(1, "A")
    val unapplyFoo = Foo.unapply(foo)
    assert(unapplyFoo.isDefined)
    assert(unapplyFoo.get._1 == new java.lang.Integer(1))
    assert(unapplyFoo.get._2 == "A")
  }

  @Test
  def testToMap() {
    val foo = Foo(1, "A")
    val map = Foo.toMap(foo)
    assert(map("a").equals(new java.lang.Integer(1)))
    assert(map("b")=="A")
    assert(map.keySet.size==2, "map.keySet has " + map.keySet.--(Set("a", "b")) + " which should be empty")
  }
}
