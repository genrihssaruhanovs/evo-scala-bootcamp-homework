package evo.homework

import scala.annotation.tailrec

object Main {
  def lcm(a: Int, b: Int): Int =
    if (a == b && a == 0) 0 else Math.abs(a * b) / gcd(a, b)

  @tailrec
  def gcd(a: Int, b: Int): Int =
    if (b == 0) a else gcd(b, a % b)
}
