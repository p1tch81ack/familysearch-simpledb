package org.familysearch.joetools.simpledb;

import junit.framework.TestCase;

import java.util.LinkedList;
import java.util.List;


public class TestSimpleTableJava extends TestCase{
    class Foo {
        private Integer a;
        private String b;

//    static Companion<Foo> companion = new Companion<Foo>(Foo.class);

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
    public void testGetAllRows() {
        Foo foo = new Foo(1, "A");
        List<Foo> fooList = new LinkedList<Foo>();
        fooList.add(foo);
        SimpleTable<Foo> fooTable = new SimpleTable<Foo>(fooList, Foo.class);

        scala.collection.immutable.List<Foo> matchingRows = fooTable.getMatchingRows(new RowSpecifier());
        assertEquals(1, matchingRows.length());
        assertEquals(foo.getB(), matchingRows.apply(0).getB());
        assertEquals(foo.getA(), matchingRows.apply(0).getA());
    }
}