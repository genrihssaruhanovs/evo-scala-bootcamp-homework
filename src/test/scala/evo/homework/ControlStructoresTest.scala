package evo.homework

import evo.homework.ControlStructures._
import org.scalatest.funsuite.AnyFunSuite

class ControlStructoresTest extends AnyFunSuite {
  test("ClassesAndTraits.process.divide") {
    assert(process("divide 5 2") == "5 divided by 2 is 2.5")
    assert(process("divide 4 5") == "4 divided by 5 is 0.8")
    assert(process("divide 5 0") == "Error: Cannot divide by 0")
    assert(process("divide 5 2 5") == "Error: Divide command must only have dividend and divisor")
  }

  test("ClassesAndTraits.process.sum") {
    assert(process("sum 5 5 6 8.5") == "the sum of 5 5 6 8.5 is 24.5")
    assert(process("sum 0 0 0") == "the sum of 0 0 0 is 0")
  }

  test("ClassesAndTraits.process.max") {
    assert(process("max 4 -3 -17") == "the maximum of 4 -3 -17 is 4")
    assert(process("max 20 5 2") == "the maximum of 20 5 2 is 20")
  }

  test("ClassesAndTraits.process.min") {
    assert(process("min 4 -3 -17") == "the minimum of 4 -3 -17 is -17")
    assert(process("min 0 5 2") == "the minimum of 0 5 2 is 0")
  }

  test("ClassesAndTraits.process.average") {
    assert(process("average 4 3 8.5 4") == "the average of 4 3 8.5 4 is 4.875")
    assert(process("average 0 5 2") == "the average of 0 5 2 is 2.333")
  }

  test("ClassesAndTraits.process.issues") {
    assert(process("bubble 4 3 8.5 4") == "Error: Wrong command")
    assert(process("max 5 2 bubble") == "Error: Inconsistent parameters found: bubble")
    assert(process("max 5 2 bubble 0.2 s") == "Error: Inconsistent parameters found: bubble, s")
    assert(process("max") == "Error: No parameters for calculation")
    assert(process("max    ") == "Error: No parameters for calculation")
    assert(process("max  5   2 1  ") == "the maximum of 5 2 1 is 5")
  }
  //   divide 4 5
  // which should output "4 divided by 5 is 0.8"

  //   sum 5 5 6 8.5
  // which should output "the sum of 5 5 6 8.5 is 24.5"

  //   average 4 3 8.5 4
  // which should output "the average of 4 3 8.5 4 is 4.875"

  //   min 4 -3 -17
  // which should output "the minimum of 4 -3 -17 is -17"

  //   max 4 -3 -17
  // which should output "the maximum of 4 -3 -17 is 4"

}
