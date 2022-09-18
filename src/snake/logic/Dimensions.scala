package snake.logic

// you can alter this file!

case class Dimensions(width : Int, height : Int) {
  // scanned from left to right, top to bottom
  def allPointsInside : Seq[Point] =
    for(y <- 0 until height; x <- 0 until width)
      yield Point(x,y)

  def getLocationIfPointOutOfBounds(p : Point) : Point = {
    if (p.x > width ) return new Point(0, p.y)
    if (p.x < 0 ) return  new Point(width, p.y)
    if (p.y > height ) return  new Point(p.x, 0)
    if (p.y < 0 ) return  new Point(p.x, height)
    return null
  }
}
