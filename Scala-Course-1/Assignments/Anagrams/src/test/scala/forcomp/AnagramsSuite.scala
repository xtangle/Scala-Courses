package forcomp

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import Anagrams._

@RunWith(classOf[JUnitRunner])
class AnagramsSuite extends FunSuite  {

  test("wordOccurrences: <Empty String>") {
    assert(wordOccurrences("") === List())
  }

  test("wordOccurrences: abcd") {
    assert(wordOccurrences("abcd") === List(('a', 1), ('b', 1), ('c', 1), ('d', 1)))
  }

  test("wordOccurrences: Robert") {
    assert(wordOccurrences("Robert") === List(('b', 1), ('e', 1), ('o', 1), ('r', 2), ('t', 1)))
  }

  test("wordOccurrences: Abacadabra") {
    assert(wordOccurrences("Abacadabra") === List(('a', 5), ('b', 2), ('c', 1), ('d', 1), ('r', 1)))
  }

  test("sentenceOccurrences: abcd e") {
    assert(sentenceOccurrences(List("abcd", "e")) === List(('a', 1), ('b', 1), ('c', 1), ('d', 1), ('e', 1)))
  }

  test("sentenceOccurrences: Can I Have A Cookie") {
    assert(sentenceOccurrences(List("Can", "I", "Have", "A", "Cookie")) ===
      List(('a', 3), ('c', 2), ('e', 2), ('h', 1), ('i', 2), ('k', 1), ('n', 1), ('o', 2), ('v', 1)))
  }

  test("dictionaryByOccurrences.get: eat") {
    assert(dictionaryByOccurrences.get(List(('a', 1), ('e', 1), ('t', 1))).map(_.toSet) === Some(Set("ate", "eat", "tea")))
  }

  test("dictionaryByOccurrences.get: aaa") {
    assert(dictionaryByOccurrences.get(List(('a', 3))) === None)
  }

  test("word anagrams: married") {
    assert(wordAnagrams("married").toSet === Set("married", "admirer"))
  }

  test("word anagrams: player") {
    assert(wordAnagrams("player").toSet === Set("parley", "pearly", "player", "replay"))
  }

  test("subtract: asa - asa") {
    val asa = List(('a', 2), ('s', 1))
    assert(subtract(asa, asa) === List())
  }

  test("subtract: Linux her - her") {
    val linuxHer = sentenceOccurrences(List("Linux", "her"))
    val linux = wordOccurrences("linux")
    val her = wordOccurrences("her")
    assert(subtract(linuxHer, her) === linux)
  }

  test("subtract: lard - r") {
    val lard = List(('a', 1), ('d', 1), ('l', 1), ('r', 1))
    val r = List(('r', 1))
    val lad = List(('a', 1), ('d', 1), ('l', 1))
    assert(subtract(lard, r) === lad)
  }

  test("combinations: []") {
    assert(combinations(Nil) === List(Nil))
  }

  test("combinations: abba") {
    val abba = List(('a', 2), ('b', 2))
    val abbacomb = List(
      List(),
      List(('a', 1)),
      List(('a', 2)),
      List(('b', 1)),
      List(('a', 1), ('b', 1)),
      List(('a', 2), ('b', 1)),
      List(('b', 2)),
      List(('a', 1), ('b', 2)),
      List(('a', 2), ('b', 2))
    )
    assert(combinations(abba).toSet === abbacomb.toSet)
  }

  test("combinations: aaa") {
    val aaa = List(('a', 3))
    val comb = List(
      List(),
      List(('a', 1)),
      List(('a', 2)),
      List(('a', 3))
    )
    assert(combinations(aaa).toSet === comb.toSet)
  }

  test("sentence anagrams: []") {
    val sentence = List()
    assert(sentenceAnagrams(sentence) === List(Nil))
  }

  test("sentence anagrams: the") {
    val sentence = List("the")
    assert(sentenceAnagrams(sentence) === List(List("the")))
  }

  test("sentence anagrams: he re") {
    val sentence = List("he", "re")
    val anas = List(
      List("he", "re"),
      List("re", "he"),
      List("here")
    )
    assert(sentenceAnagrams(sentence).toSet === anas.toSet)
  }

  test("sentence anagrams: Linux her") {
    val sentence = List("Linux", "her")
    val anas = List(
      List("run", "helix"),
      List("urn", "helix"),
      List("in", "hurl", "ex"),
      List("in", "ex", "hurl"),
      List("Linux", "her"),
      List("hurl", "in", "ex"),
      List("hurl", "ex", "in"),
      List("ex", "in", "hurl"),
      List("ex", "hurl", "in"),
      List("her", "Linux"),
      List("helix", "run"),
      List("helix", "urn")
    )
    assert(sentenceAnagrams(sentence).toSet === anas.toSet)
  }

  test("sentence anagrams: Linux rulez") {
    val sentence = List("Linux", "rulez")
    val anas = List(
      List("Rex", "Lin", "Zulu"),
      List("nil", "Zulu", "Rex"),
      List("Rex", "nil", "Zulu"),
      List("Zulu", "Rex", "Lin"),
      List("null", "Uzi", "Rex"),
      List("Rex", "Zulu", "Lin"),
      List("Uzi", "null", "Rex"),
      List("Rex", "null", "Uzi"),
      List("null", "Rex", "Uzi"),
      List("Lin", "Rex", "Zulu"),
      List("nil", "Rex", "Zulu"),
      List("Rex", "Uzi", "null"),
      List("Rex", "Zulu", "nil"),
      List("Zulu", "Rex", "nil"),
      List("Zulu", "Lin", "Rex"),
      List("Lin", "Zulu", "Rex"),
      List("Uzi", "Rex", "null"),
      List("Zulu", "nil", "Rex"),
      List("rulez", "Linux"),
      List("Linux", "rulez")
    )
    assert(sentenceAnagrams(sentence).toSet === anas.toSet)
  }

}
