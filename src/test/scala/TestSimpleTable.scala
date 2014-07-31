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
    /*val table = */ new SimpleTable[Foo](fooList)
  }

  @Test
  def testTableCreationFromJavaCollection() {
    val fooList = new java.util.LinkedList[Foo]()
    /*val table = */new SimpleTable[Foo](fooList, classOf[Foo])
    val foo=Foo(1, "A")
    /*val fooUnapply: Option[_<:Product] =*/ Foo.unapply(foo)
  }

}
