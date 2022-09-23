package snake.logic

case class SnakeBodyPart(direction: Direction, position: Point)
object SnakeBodyPart{
  def toPoints(snakeBody: Vector[SnakeBodyPart]): Vector[Point] = {
    for( aSnakeBodyPart <- snakeBody )
      yield Point(aSnakeBodyPart.position.x, aSnakeBodyPart.position.y)
  }
}

