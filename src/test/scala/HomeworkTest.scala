import evo.homework.Basics
import org.scalatest.FunSuite

class HomeworkTest extends FunSuite {
  test ("Main.gcd"){
    assert(Basics.gcd(3,5) == 1)
    assert(Basics.gcd(0,5) == 5)
    assert(Basics.gcd(24,12) == 12)
    assert(Basics.gcd(0,0) == 0)
  }

  test ("Main.lcm"){
    assert(Basics.lcm(3,0) == 0)
    assert(Basics.lcm(3,5) == 15)
    assert(Basics.lcm(0,0) == 0)
    assert(Basics.lcm(1,5) == 5)
  }
}
