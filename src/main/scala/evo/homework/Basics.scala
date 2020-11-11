package evo.homework

import cats.effect.IO

import scala.annotation.tailrec
import cats.implicits._
object Basics {
  def lcm(a: Int, b: Int): Int =
    if (a == b && a == 0) 0 else Math.abs(a * b) / gcd(a, b)

  @tailrec
  def gcd(a: Int, b: Int): Int =
    if (b == 0) a else gcd(b, a % b)



  def main(args: Array[String]): Unit = {
    val testIO = IO(5)
    val testIO2 = IO(testIO)

    val testIO3 = IO.suspend(testIO2)


    println((0 to 10).filter(_ % 2 == 0).sum)
  }
}
