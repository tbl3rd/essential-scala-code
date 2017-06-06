package adt.shape

final case class Color(r: Double, g: Double, b: Double)

sealed trait Shape

final case class Circle(r: Double, c: Color) extends Shape {
  def area = Math.PI * r * r
}

final case class Rect(h: Double, w: Double, c: Color) extends Shape {
  def area = h * w
}

object Main extends App {
  val shape1 = Circle(10, Color(1, 0, 0))
  val shape2 = Rect(3, 5, Color(0, 1, 0))

  println(shape1)
  println(shape2)

  println(shape1, shape1.area)
  println(shape2, shape2.area)

  def area(shape: Shape): Double = shape match {
    case Circle(r, _) => Math.PI * r * r
    case Rect(h, w, _) => h * w
  }

  // println(shape1 + " " + area(shape1))
  // println(shape2 + " " + area(shape2))
}
