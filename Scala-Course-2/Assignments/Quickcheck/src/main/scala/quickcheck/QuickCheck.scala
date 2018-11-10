package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = frequency(
    (1, const(empty)),
    (4, for {
      i <- arbitrary[Int]
      h <- genHeap
    } yield insert(i, h))
  )
  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  def genListBetween[T](min: Int, max: Int)(g: Gen[T]): Gen[List[T]] =
    for {
      numElems <- Gen.choose(min, max)
      elems <- Gen.listOfN(numElems, g)
    } yield elems

  property("inserting the min element does not change the min element") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("min element of a heap with a single element is that element") = forAll { (a: Int) =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  property("deleting the min element of a heap with a single element returns the empty heap") = forAll { (a: Int) =>
    val h = insert(a, empty)
    deleteMin(h) == empty
  }

  property("min element of a heap with two elements returns the lesser of the two elements") = forAll { (a: Int, b: Int) =>
    val h = insert(b, insert(a, empty))
    findMin(h) == (a min b)
  }

  property("repeatedly finding the min element of a heap returns a sorted list of elements") = forAll { (h: H) =>
    val l = getElements(h)
    classify(l.length > 5, "large", "small") {
      l == l.sorted.reverse
    }
  }

  property("min element of the meld of two heaps returns lesser min element of one of the heaps") = forAll { (h1: H, h2: H) =>
    (!isEmpty(h1) && !isEmpty(h2)) ==> {
      val (m1, m2) = (findMin(h1), findMin(h2))
      findMin(meld(h1, h2)) == (m1 min m2)
    }
  }

  property("repeatedly finding the min element of a heap returns same list of elements we inserted into it") =
    forAll(genListBetween(3, 8)(arbitrary[Int]) suchThat (l => l == l.distinct)) { (l: List[Int]) =>
    def getList(h1: H, acc: List[Int]): List[Int] =
      if (!isEmpty(h1)) {
        val m = findMin(h1)
        getList(deleteMin(h1), m :: acc)
      } else acc

    classify(l.length > 5, "large", "small") {
      val h = insertElements(empty, l)
      val elems = getElements(h)
      l.sorted == elems.sorted
    }
  }

  def insertElements(h: H, elem: List[Int]): H = elem match {
    case Nil => h
    case x :: xs => insertElements(insert(x, h), xs)
  }

  def getElements(h: H): List[Int] = {
    def getElementsAcc(h1: H, acc: List[Int]): List[Int] =
      if (!isEmpty(h1)) {
        val m = findMin(h1)
        getElementsAcc(deleteMin(h1), m :: acc)
      } else acc
    getElementsAcc(h, List())
  }

}
