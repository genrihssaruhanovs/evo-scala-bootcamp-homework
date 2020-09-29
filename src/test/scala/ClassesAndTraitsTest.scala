import evo.homework.ClassesAndTraits._
import org.scalatest.FunSuite

class ClassesAndTraitsTest extends FunSuite {
  val round: (Double, Int) => Double = (number, digits) => (number * Math.pow(10, digits)).round / Math.pow(10, digits)

  test ("ClassesAndTraits.Square"){

    val square: Square = Square(1,2,5)
    assert(square.minX == -1.5)
    assert(square.minY == -0.5)
    assert(square.perimeter == 20)
    assert(square.area == 25)

    val movedSquare = square.move(2,1)
    assert(movedSquare.isInstanceOf[Square])
    assert(movedSquare.minX == 0.5)
    assert(movedSquare.minY == 0.5)
    assert(movedSquare.perimeter == 20)
    assert(movedSquare.area == 25)
  }

  test ("ClassesAndTraits.Circle"){
    val circle: Circle = Circle(1,2,5)
    assert(circle.minX == -4)
    assert(circle.minY == -3)
    assert(round(circle.perimeter, 2) == 31.42)
    assert(round(circle.area, 2) == 78.54)

    val movedCircle: Circle = circle.move(3,4)
    assert(movedCircle.minX == -1)
    assert(movedCircle.minY == 1)
    assert(round(movedCircle.perimeter, 2) == 31.42)
    assert(round(movedCircle.area, 2) == 78.54)
  }

  test ("ClassesAndTraits.Cuboid"){
    val cuboid: Cuboid = Cuboid(2, Rectangle(1, 2, 4, 5), 4)

    assert(cuboid.minX == -1)
    assert(cuboid.maxY == 4.5)
    assert(cuboid.minZ == -2)
    assert(cuboid.volume == 80)
    assert(cuboid.surfaceArea == 112)

    val movedCuboid = cuboid.move(1,2,3)

    assert(movedCuboid.minX == 0)
    assert(movedCuboid.maxY == 6.5)
    assert(movedCuboid.minZ == 1)
    assert(movedCuboid.volume == 80)
    assert(movedCuboid.surfaceArea == 112)
  }

  test ("ClassesAndTraits.Pyramid"){
    val pyramid: Pyramid = Pyramid(2, Rectangle(1, 2, 2, 3), 4)

    assert(pyramid.minX == 0)
    assert(pyramid.maxY == 3.5)
    assert(pyramid.minZ == -2)
    assert(pyramid.volume == 8)
    assert(round(pyramid.surfaceArea, 2) == 26.91)
  }
}
