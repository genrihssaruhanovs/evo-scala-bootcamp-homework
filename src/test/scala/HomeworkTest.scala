import evo.homework.Main
import org.scalatest.FunSuite

class HomeworkTest extends FunSuite {
  test ("Main.gcd"){
    assert(Main.gcd(3,5) == 1)
    assert(Main.gcd(0,5) == 5)
    assert(Main.gcd(24,12) == 12)
    assert(Main.gcd(0,0) == 0)
  }

  test ("Main.lcm"){
    assert(Main.lcm(3,0) == 0)
    assert(Main.lcm(3,5) == 15)
    assert(Main.lcm(0,0) == 0)
    assert(Main.lcm(1,5) == 5)
  }
}
