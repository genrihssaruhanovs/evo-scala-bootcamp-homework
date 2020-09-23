package evo.homework

object Main{
  def main(args: Array[String]): Unit = {
    println(gcd(25,20))
    println(lcm(4,6))
  }

  def lcm(a: Int, b: Int): Int = Math.abs(a * b) / gcd(a, b)
  def gcd(a: Int, b: Int): Int = if (a % b == 0 ) b else gcd(b, a % b)
}
