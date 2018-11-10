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
    * val s1 = singletonSet(1)
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
    val s1: Set = singletonSet(1)
    val s2: Set = singletonSet(2)
    val s3: Set = singletonSet(3)
    val emptySet: Set = _ => false
    val allNumbers: Set = _ => true
    val isTheNumber: Int => Set = x => singletonSet(x)
    val isNotTheNumber: Int => Set = x => !isTheNumber(x)(_)
    val isDivisibleBy: Int => Set = x => y => y % x == 0
    val isNotDivisibleBy: Int => Set = x => !isDivisibleBy(x)(_)
    val isLessThanOrEqualTo: Int => Set = x => y => y <= x
    val isGreaterThanOrEqualTo: Int => Set = x => y => !isLessThanOrEqualTo(x)(y) || isTheNumber(x)(y)
    val isBetweenIncl: (Int, Int) => Set = (a, b) => x => isGreaterThanOrEqualTo(a)(x) || isLessThanOrEqualTo(b)(x)
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
      assert(contains(s1, 1), "Singleton s1 should contain 1")
    }
  }

  test("singletonSet(2) does not contain 1") {
    new TestSets {
      assert(!contains(s2, 1), "Singleton s2 should not contain 1")
    }
  }

  test("union contains all elements of each set") {
    new TestSets {
      private val s = union(s1, s2)
      assert(contains(s, 1), "Union should contain 1")
      assert(contains(s, 2), "Union should contain 2")
      assert(!contains(s, 3), "Union should not contain 3")
    }
  }

  test("intersect when both sets have elements in common") {
    new TestSets {
      private val s = intersect(union(s1, s2), union(s2, s3))
      assert(!contains(s, 1), "Intersect should not contain 1")
      assert(contains(s, 2), "Intersect should contain 2")
      assert(!contains(s, 3), "Intersect should not contain 3")
    }
  }

  test("intersect when both sets are disjoint") {
    new TestSets {
      private val s = intersect(s1, s2)
      assert(!contains(s, 1), "Intersect should not contain 1")
      assert(!contains(s, 2), "Intersect should not contain 2")
      assert(!contains(s, 3), "Intersect should not contain 3")
    }
  }

  test("difference contains elements only in the first set") {
    new TestSets {
      private val s = diff(union(s1, s2), union(s2, s3))
      assert(contains(s, 1), "Diff should contain 1")
      assert(!contains(s, 2), "Diff should not contain 2")
      assert(!contains(s, 3), "Diff should not contain 3")
    }
  }

  test("filter contains elements which satisfy the predicate") {
    new TestSets {
      private val s = filter(union(union(s1, s2), s3), isDivisibleBy(2))
      assert(!contains(s, 1), "Filter should not contain 1")
      assert(contains(s, 2), "Filter should contain 2")
      assert(!contains(s, 3), "Filter should not contain 3")
    }
  }

  test("forall is true for an empty set") {
    new TestSets {
      assert(forall(emptySet, isDivisibleBy(2)), "Forall should be true")
    }
  }

  test("forall is true for set of numbers divisible by four and predicate is being an even number") {
    new TestSets {
      assert(forall(isDivisibleBy(4), isDivisibleBy(2)), "Forall should be true")
    }
  }

  test("forall is false for set of all numbers and predicate is not being the upper bound") {
    new TestSets {
      assert(!forall(allNumbers, isNotTheNumber(1000)), "Forall should be false")
    }
  }

  test("exists is false for an empty set") {
    new TestSets {
      assert(!exists(emptySet, isDivisibleBy(2)), "Exists should be false")
    }
  }

  test("exists is true for set of nonzero numbers and predicate is being divisible by 968") {
    new TestSets {
      assert(exists(isNotTheNumber(0), isDivisibleBy(968)), "Exists should be true")
    }
  }

  test("exists is false for set of nonzero numbers and predicate is being divisible by 1001") {
    new TestSets {
      assert(!exists(isNotTheNumber(0), isDivisibleBy(1001)), "Exists should be false")
    }
  }

  test("map when the set is empty") {
    new TestSets {
      assert(!exists(map(emptySet, identity), allNumbers), "Map should be the empty set")
    }
  }

  test("map when the set is not empty") {
    new TestSets {
      private val s = map(isBetweenIncl(-100, 100), x => 2 * x)
      private val p: Int => Boolean = x => isBetweenIncl(-200, 200)(x) && isDivisibleBy(2)(x)
      assert(forall(s, p), "Map should contain all even numbers between -200 and 200")
      assert(!exists(s, x => !p(x)), "Map should not contain numbers that are not even numbers between -200 and 200")
    }
  }

}