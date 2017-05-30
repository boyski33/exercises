//Боян Кушлев F75128

package ex2

sealed trait Shape
case class Triangle(a: Int, b: Int, c: Int, h: Int) extends Shape // h represent the height against the longest side of the triangle
case class Rectangle(a: Int, b: Int) extends Shape
case class Trapezoid(a: Int, b: Int, h: Int) extends Shape
case class Cube() extends Shape

class Architect {

  /*
   *  Finds the max element from given list of integers.
   *  The result is wrapped in an Option instance. The Option has two forms:
   *   - None if no element satisfies the search criteria (for example, in case an empty list is provided)
   *   - Some(x), where x if the searched element. In this case, it can be acquired with the method get. Example:
   *     val o: Option[Int] = Some(6)
   *     val n: Int = o.get
   */
  def max(xs: List[Int]): Option[Int] = {
    def loop(xs: List[Int], m: Int): Option[Int] = {
      if(xs.isEmpty) Some(m)
      else {
        if(xs.head > m) loop(xs.tail, xs.head)
        else loop(xs.tail, m)
      }
    }

    if(xs.isEmpty) None
    else loop(xs, Int.MinValue)
  }

  // Determines the type of given triangle: "rectangular", "equilateral", "isosceles", "random"
  def triangleType(t: Triangle): String = {
    if (t.a == t.b || t.b == t.c || t.a == t.c) "isosceles"
    else if (t.a == t.b && t.b == t.c && t.a == t.c) "equilateral"
    else {
      val a2 = t.a * t.a
      val b2 = t.b * t.b
      val c2 = t.c * t.c
      if ((a2 == b2 + c2) || (b2 == a2 + c2) || (c2 == a2 + b2)) "rectangular"
      else "random"
    }
  }

  /*
   * Calculates the area of the provided shape, by using these formulae:
   *  - Rectangular triangle: a * b / 2, where a and b are cathetus
   *  - Any triangle except rectangular: x * h / 2, where x is the largest side of the triangle and h is the opposite height
   *  - Rectangle: a * b, where a and b are both sides
   *  - Trapezoid: (a + b) * h / 2, where a and b are the parallel sides and h is the height between them
   *  - Cube: always return -1
   *
   *  Hint: for triangles use the max function
   */
  def area(s: Shape): Double = s match {
    case t: Triangle => triangleType(t) match {
      case "rectangular" => max(List(t.a, t.b, t.c)).get match {
        case t.a => t.b * t.c / 2.0
        case t.b => t.c * t.a / 2.0
        case t.c => t.a * t.b / 2.0
      }
      case _ => max(List(t.a, t.b, t.c)).get match {
        case t.a => t.a * t.h / 2.0
        case t.b => t.b * t.h / 2.0
        case t.c => t.c * t.h / 2.0
      }
    }
    case r: Rectangle => r.a * r.b
    case t: Trapezoid => (t.a + t.b) * t.h / 2.0
    case c: Cube => -1
  }

  /*
   *  Returns the number of rectangular triangles in given list of shapes
   *
   *  Hint: use the triangleType function
   */
  def findRectangulars(shapes: List[Shape]): Int = {
    def iter(shapes: List[Shape], n: Int): Int = {
      if(shapes.isEmpty) n
      else shapes.head match {
        case t: Triangle => triangleType(t) match {
          case "rectangular" => iter(shapes.tail, n+1)
          case _ => iter(shapes.tail, n)
        }
        case s: Shape => iter(shapes.tail, n)
      }
    }
    iter(shapes, 0)
  }
}
