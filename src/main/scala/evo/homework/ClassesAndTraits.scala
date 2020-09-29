package evo.homework

object ClassesAndTraits {

  sealed trait Located2D {
    def x: Double
    def y: Double
  }

  sealed trait Located3D{
    def z: Double
  }

  sealed trait Bounded2D {
    def minX: Double
    def maxX: Double
    def minY: Double
    def maxY: Double
  }

  sealed trait Bounded3D {
    def minZ: Double
    def maxZ: Double
  }

  sealed trait Movable2D {
    def move(dx: Double, dy: Double): Shape2D
  }

  sealed trait Movable3D {
    def move(dx: Double, dy: Double, dz: Double): Shape3D
  }

  sealed trait Shape extends Located2D with Bounded2D

  sealed trait Shape2D extends Shape with Movable2D{
    def perimeter: Double
    def area: Double
  }

  sealed trait Shape3D extends Shape with Located3D with Bounded3D with Movable3D {
    def baseFigure: Shape2D
    def volume: Double
    def surfaceArea: Double
    override def minX: Double = baseFigure.minX
    override def maxX: Double = baseFigure.maxX
    override def minY: Double = baseFigure.minY
    override def maxY: Double = baseFigure.maxY
    override def x: Double = baseFigure.x
    override def y: Double = baseFigure.y
  }


  final case class Point(x: Double, y: Double) extends Shape2D {
    override def minX: Double = x
    override def maxX: Double = x
    override def minY: Double = y
    override def maxY: Double = y

    override def move(dx: Double, dy: Double): Point = Point(x + dx, y + dy)
    override def perimeter: Double = 1
    override def area: Double = 1
  }

   case class Rectangle(x: Double, y: Double, width: Double, length: Double) extends Shape2D {
    override def minX: Double = x - width / 2
    override def maxX: Double = x + width / 2
    override def minY: Double = y - length / 2
    override def maxY: Double = y + length / 2


    override def move(dx: Double, dy: Double): Rectangle = Rectangle(x + dx, y + dy, length, width)
    override def perimeter: Double =  (length * width ) * 2
    override def area: Double = width * length
  }

  final case class Circle(x: Double, y: Double, radius: Double) extends Shape2D {
    override def minX: Double = x - radius
    override def maxX: Double = x + radius
    override def minY: Double = y - radius
    override def maxY: Double = y + radius
    override def move(dx: Double, dy: Double): Circle = Circle(x + dx, y + dy, radius)

    override def perimeter: Double = 2 * Math.PI * radius
    override def area: Double = Math.PI * Math.pow(radius, 2)
  }

  final case class Square(x: Double, y: Double, side: Double) extends Shape2D{
    override def minX: Double = x - side / 2
    override def maxX: Double = x + side / 2
    override def minY: Double = y - side / 2
    override def maxY: Double = y + side / 2
    override def move(dx: Double, dy: Double): Square = Square(x + dx, y + dy, side)

    override def perimeter: Double =  side * 4

    override def area: Double = Math.pow(side, 2)
  }

  final case class IsoscelesTriangle(x: Double, y: Double, base: Double, altitude: Double) extends Shape2D {
    override def minX: Double = x - base / 2
    override def maxX: Double = x + base / 2
    override def minY: Double = y - altitude / 2
    override def maxY: Double = y + altitude / 2

    def side: Double = Math.sqrt(Math.pow(base / 2, 2) + Math.pow(altitude, 2))

    override def perimeter: Double = side * 2 + altitude
    override def move(dx: Double, dy: Double): IsoscelesTriangle = IsoscelesTriangle(x + dx, y + dy, base, altitude)
    override def area: Double = base / 2 * altitude
  }

  final case class Point3D(z: Double, baseFigure: Point) extends Shape3D{
    override def volume: Double = 1
    override def surfaceArea: Double = 1
    override def minZ: Double = z
    override def maxZ: Double = z

    override def move(dx: Double, dy: Double, dz: Double): Point3D = Point3D(z+dz, Point(x+dx, y+dy))
  }

  final case class Cube(z: Double, baseFigure: Square, height: Double) extends Shape3D{
    override def volume: Double = baseFigure.area * height
    override def surfaceArea: Double = baseFigure.area * 6
    override def minZ: Double = z - height
    override def maxZ: Double = z + height

    override def move(dx: Double, dy: Double, dz: Double): Cube = Cube(z + dz, Square(x+ dx, y+ dy, baseFigure.side), height)
  }

  final case class Cuboid(z: Double, baseFigure: Rectangle, height: Double) extends Shape3D{
    override def volume: Double = baseFigure.area * height
    override def surfaceArea: Double = 2 * (baseFigure.area + baseFigure.width * height + baseFigure.length * height)
    override def minZ: Double = z - height
    override def maxZ: Double = z + height

    override def move(dx: Double, dy: Double, dz: Double): Cuboid = Cuboid(z + dz, Rectangle(x+ dx, y+ dy, baseFigure.width, baseFigure.length), height)
  }

  final case class Sphere(z: Double, baseFigure: Circle) extends Shape3D{
    override def minZ: Double = z - baseFigure.radius
    override def maxZ: Double = z + baseFigure.radius
    override def volume: Double = 4/3 * baseFigure.area * baseFigure.radius // = (4/3 * PI * r^3 ) baseFigure.area = PI * r^2
    override def surfaceArea: Double = 2 * baseFigure.perimeter             // = (4 * PI * r) , baseFigure.perimeter = 2 * PI * r

    override def move(dx: Double, dy: Double, dz: Double): Sphere = Sphere(z + dz, Circle(x+ dx, y+ dy, baseFigure.radius))
  }

  final case class Tetrahedron(z: Double, baseFigure: IsoscelesTriangle, height: Double) extends Shape3D{
    override def minZ: Double = z - height
    override def maxZ: Double = z + height
    override def volume: Double = baseFigure.area * height / 3
    override def surfaceArea: Double = ???

    override def move(dx: Double, dy: Double, dz: Double): Tetrahedron = Tetrahedron(z + dz, IsoscelesTriangle(x + dx, y + dy, baseFigure.base, baseFigure.altitude), height)
  }

  final case class Pyramid(z: Double, baseFigure: Rectangle, height: Double) extends Shape3D{
    override def minZ: Double = z - height
    override def maxZ: Double = z + height
    override def volume: Double = baseFigure.area * height / 3
    override def surfaceArea: Double =
      baseFigure.length * baseFigure.width + baseFigure.length * Math.sqrt(Math.pow(baseFigure.width / 2, 2) + Math.pow(height,2)) + baseFigure.width * Math.sqrt(Math.pow(baseFigure.length / 2, 2) + Math.pow(height,2))

    override def move(dx: Double, dy: Double, dz: Double): Pyramid = Pyramid(z + dz, Rectangle(x + dx, y + dy, baseFigure.length, baseFigure.width), height)
  }

  object Origin extends Located2D with Located3D{
    override def x: Double = 0
    override def y: Double = 0
    override def z: Double = 0
  }
}
