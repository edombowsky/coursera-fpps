package funsets

import org.scalatest.FunSuite


import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

/**
 * This class is a test suite for the methods in object FunSets. To run
 * the test suite, you can either:
 *  - run the "test" command in the SBT console
 *  - right-click the file in eclipse and chose "Run As" - "JUnit Test"
 */
@RunWith(classOf[JUnitRunner])
class FunSetSuite extends FunSuite {

  /**
   * Link to the scaladoc - very clear and detailed tutorial of FunSuite
   *
   * http://doc.scalatest.org/1.9.1/index.html#org.scalatest.FunSuite
   *
   * Operators
   *  - test
   *  - ignore
   *  - pending
   */

  /**
   * Tests are written using the "test" operator and the "assert" method.
   */
  // test("string take") {
  //   val message = "hello, world"
  //   assert(message.take(5) == "hello")
  // }

  /**
   * For ScalaTest tests, there exists a special equality operator "===" that
   * can be used inside "assert". If the assertion fails, the two values will
   * be printed in the error message. Otherwise, when using "==", the test
   * error message will only say "assertion failed", without showing the values.
   *
   * Try it out! Change the values so that the assertion fails, and look at the
   * error message.
   */
  // test("adding ints") {
  //   assert(1 + 2 === 3)
  // }


  import FunSets._

  test("contains is implemented") {
    assert(contains(x => true, 100))
  }

  /**
   * When writing tests, one would often like to re-use certain values for multiple
   * tests. For instance, we would like to create an Int-set and have multiple test
   * about it.
   *
   * Instead of copy-pasting the code for creating the set into every test, we can
   * store it in the test class using a val:
   *
   *   val s1 = singletonSet(1)
   *
   * However, what happens if the method "singletonSet" has a bug and crashes? Then
   * the test methods are not even executed, because creating an instance of the
   * test class fails!
   *
   * Therefore, we put the shared values into a separate trait (traits are like
   * abstract classes), and create an instance inside each test method.
   *
   */

  trait TestSets {
    val s1 = singletonSet(1)
    val s2 = singletonSet(2)
    val s3 = singletonSet(3)

    val t1 = Set(1,3,4,5,7,1000)
    val t2 = Set(1,2,3,4)
    val t3 = Set(1,3)
  }

  /**
   * This test is currently disabled (by using "ignore") because the method
   * "singletonSet" is not yet implemented and the test would fail.
   *
   * Once you finish your implementation of "singletonSet", exchange the
   * function "ignore" by "test".
   */
  test("singletonSet(1) contains 1") {

    /**
     * We create a new instance of the "TestSets" trait, this gives us access
     * to the values "s1" to "s3".
     */
    new TestSets {
      /**
       * The string argument of "assert" is a message that is printed in case
       * the test fails. This helps identifying which assertion failed.
       */
      assert(contains(s1, 1), "Singleton")
    }
  }

  test("union contains all elements of each set") {
    new TestSets {
      val s = union(s1, s2)
      assert(contains(s, 1), "Union 1")
      assert(contains(s, 2), "Union 2")
      assert(!contains(s, 3), "Union 3")
    }
  }

  test("intersection contains all elements that are in both sets") {
    new TestSets {
      val u = union(s1, s2)
      val s = intersect(s1, u)
      assert(contains(s, 1), "Intersect 1")
      assert(!contains(s, 2), "Intersect 2")
    }
  }

  test("diff contains the set of all elements of first set that are not in the second") {
    new TestSets {
      val s = diff(s1, s2)
      assert(contains(s, 1), "diff 1")
      assert(!contains(s, 2), "diff 2")
      assert(!contains(s, 3), "diff 3")
    
      val t4 = union(s1, s2)
      val t5 = diff(t4, s3)
      assert(contains(t5, 2), "diff 4")
      assert(!contains(t5, 3), "diff 5")

      val t6 = diff(t2, t1)
      printSet(t6)
      assert(!contains(t6, 5), "diff 6")
      assert(!contains(t6, 7), "diff 7")
    
      val t7 = diff(t2, t3)
      printSet(t7)
      assert(contains(t7, 2), "diff 8")
      assert(contains(t7, 4), "diff 9")
      assert(!contains(t7, 1), "diff 10")
    }
  }

  test("filter contains only the subset for which a predicate holds.") {
    new TestSets {
      val set = union(s1, s2)
      val s = filter(set, x => x == 1)
      assert(contains(s, 1), "filter 1")
      assert(!contains(s, 2), "filter 2")
      assert(!contains(s, 1000), "filter 3")

    }
  }

  test("forall checks whether all bound integers within a set satisfy a predicate") {
    new TestSets {
      assert(forall(s1, x => true), "forall 1")
      assert(!forall(union(s1, s2), x => x == 1), "forall 2")
    }
  }

  test("exists checks whether bound integers within a set satisfy a predicate") {
    new TestSets {
      assert(exists(union(s1, s2), x => x == 1), "exists 1")
      assert(!exists(union(s1, s2), x => x == 3), "exists 2")
    }
  }
  
  test("map contains items in set transformed by a predicate") {
    new TestSets {
      
      val s = map(union(union(s1,s2),s3), x=>x*x)
      assert(contains(s,4), "map 1")
      assert(contains(s,1), "map 2")
      assert(contains(s,9), "map 3")
    }
  }

}
