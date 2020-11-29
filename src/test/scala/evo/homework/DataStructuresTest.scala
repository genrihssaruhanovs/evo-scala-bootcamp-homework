package evo.homework

import evo.homework.DataStructures._
import org.scalatest.funsuite.AnyFunSuite

class DataStructuresTest extends AnyFunSuite {
  test("DataStructures.sortConsideringEqualValues") {
    val input = Map("a" -> 1, "b" -> 2, "c" -> 4, "d" -> 1, "e" -> 0, "f" -> 2, "g" -> 2)
    val expected = List(Set("e") -> 0, Set("a", "d") -> 1, Set("b", "f", "g") -> 2, Set("c") -> 4)
    val obtained = sortConsideringEqualValues(input)
    assert(obtained == expected)

    val values = Set("a1", "a2", "b1", "c1", "c2", "d1").map { x =>
      x -> x.head.toInt
    }.toMap

    assert(
      sortConsideringEqualValues(values) == List(
        Set("a1", "a2") -> 'a'.toInt,
        Set("b1") -> 'b'.toInt,
        Set("c1", "c2") -> 'c'.toInt,
        Set("d1") -> 'd'.toInt,
      )
    )
  }
}
