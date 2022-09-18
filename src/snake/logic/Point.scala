package snake.logic

// you can alter this file!

case class Point(x : Int, y : Int) {
  // taken from Point class in sokoban example code
  def +(rhs : Point) : Point = Point(x + rhs.x, y + rhs.y)
}
