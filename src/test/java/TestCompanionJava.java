import junit.framework.TestCase;
import org.familysearch.joetools.simpledb.Companion;
import scala.collection.immutable.Map;


class Foo {
    private Integer a;
    private String b;

    static Companion<Foo> companion = new Companion<Foo>(Foo.class);

    public Foo(Integer a, String b){
        this.a = a;
        this.b =b;
    }

    public Integer getA(){
        return a;
    }

    public String getB(){
        return b;

    }
}

public class TestCompanionJava extends TestCase{

    /*
  case class Foo(a: java.lang.Integer, b: String)

  object Foo extends Companion[Foo]

     */


/*
  def testToMap() {
    val foo = Foo(1, "A")
    val map = Foo.toMap(foo)
    assert(map("a").equals(new java.lang.Integer(1)))
    assert(map("b")=="A")
    assert(map.keySet.size==2, "map.keySet has " + map.keySet.--(Set("a", "b")) + " which should be empty")
  }

 */
    public void testPacificToEastern() {
        Foo foo = new Foo(1, "A");
        Map<String, Object> map = Foo.companion.toMap(foo);
        assertEquals(foo.getA(), map.apply("a"));
        assertEquals(foo.getB(), map.apply("b"));
        assertFalse(map.keySet().size()==2);
    }
}