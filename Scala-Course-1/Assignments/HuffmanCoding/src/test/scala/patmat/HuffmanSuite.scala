package patmat

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import patmat.Huffman._

@RunWith(classOf[JUnitRunner])
class HuffmanSuite extends FunSuite {

  trait TestTrees {
    val t1 = Fork(Leaf('a', 2), Leaf('b', 3), List('a', 'b'), 5)
    val t2 = Fork(Fork(Leaf('a', 2), Leaf('b', 3), List('a', 'b'), 5), Leaf('d', 4), List('a', 'b', 'd'), 9)
  }

  test("weight of a larger tree") {
    new TestTrees {
      assert(weight(t1) === 5)
    }
  }

  test("chars of a larger tree") {
    new TestTrees {
      assert(chars(t2) === List('a', 'b', 'd'))
    }
  }

  test("string2chars(\"hello, world\")") {
    assert(string2Chars("hello, world") === List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd'))
  }

  test("times(\"hello, world\")") {
    val result = times(string2Chars("hello, world"))
    def findCount(char: Char): Int = result.find(_._1 == char).getOrElse(throw new NoSuchElementException(s"Character '$char' not found"))._2
    string2Chars("helo, wrd").zip(List(1, 1, 3, 2, 1, 1, 1, 1, 1)).foreach(p => assert(findCount(p._1) === p._2))
  }

  test("makeOrderedLeafList for some frequency table") {
    assert(makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))) === List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 3)))
  }

  test("combine of empty list") {
    assert(combine(List()) === List())
  }

  test("combine of some leaf list") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assert(combine(leaflist) === List(Fork(Leaf('e', 1), Leaf('t', 2), List('e', 't'), 3), Leaf('x', 4)))
  }

  test("until of some leaf list") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assert(until(singleton, combine)(leaflist) ===
      Fork(Fork(Leaf('e', 1), Leaf('t', 2), List('e', 't'), 3), Leaf('x', 4), List('e', 't', 'x'), 7))
  }

  test("createCodeTree of some char list") {
    new TestTrees {
      createCodeTree(List('a', 'd', 'b', 'd', 'd', 'a', 'b', 'd', 'b')) match {
        case Fork(l, r, c, w) =>
          assert(List(l, r).toSet === List(t2.left, t2.right).toSet && c.toSet === t2.chars.toSet && w === t2.weight)
        case _ => fail("createCodeTree should return a Fork type")
      }
    }
  }

  test("decode and encode a very short text should be identity") {
    new TestTrees {
      assert(decode(t1, encode(t1)("ab".toList)) === "ab".toList)
    }
  }

  test("decode and encode a longer text should be identity") {
    new TestTrees {
      assert(decode(t2, encode(t2)("daabada".toList)) === "daabada".toList)
    }
  }

  test("codeBits for a short code table") {
    val codeTable = List(('a', List(0, 1)), ('b', List(1)), ('c', List(0, 0)))
    assert(codeBits(codeTable)('c') === List(0, 0))
  }

  test("convert for t2") {
    new TestTrees {
      assert(convert(t2).toSet === List(('a', List(0, 0)), ('b', List(0, 1)), ('d', List(1))).toSet)
    }
  }

  test("decode and quickEncode a longer text should be identity") {
    new TestTrees {
      assert(decode(t2, quickEncode(t2)("daabada".toList)) === "daabada".toList)
    }
  }

}
