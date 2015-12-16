package org.familysearch.joetools.simpledb;

import junit.framework.TestCase;

import java.util.LinkedList;
import java.util.List;
import java.util.Set;

public class TestSimpleTableJava extends TestCase {
  class Foo {
    private Integer a;
    private String b;

    public Foo(Integer a, String b) {
      this.a = a;
      this.b = b;
    }

    public Integer getA() {
      return a;
    }

    public String getB() {
      return b;

    }
  }

  public void testGetAllRows() {
    assertAllRows(createTableWithObviousFieldMapper());
  }

  private void assertFieldNames(SimpleTable<Foo> fooSimpleTable) {
    scala.collection.immutable.Set<String> fieldNames = fooSimpleTable.getFieldNames();
    assertTrue(fieldNames.contains("a"));
    assertTrue(fieldNames.contains("b"));
    assertTrue(fieldNames.size() == 2);
  }

  private void assertAllRows(SimpleTable<Foo> fooTable) {
    scala.collection.immutable.List<Foo> matchingRows = fooTable.getMatchingRows(new RowSpecifier());
    assertEquals(1, matchingRows.length());
    assertEquals("A", matchingRows.apply(0).getB());
    assertEquals(new Integer(1), matchingRows.apply(0).getA());
  }

  private SimpleTable<Foo> createTableWithObviousFieldMapper() {
    return new SimpleTable<Foo>(createFooList(), Foo.class);
  }

  private List<Foo> createFooList() {
    Foo foo = new Foo(1, "A");
    List<Foo> fooList = new LinkedList<Foo>();
    fooList.add(foo);
    return fooList;
  }
}