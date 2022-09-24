package snake.logic
import engine.random.{RandomGenerator, ScalaRandomGen}

import scala.:+
import scala.collection.immutable.Queue

//import java.util.random.RandomGenerator



class GameLogic(val random: RandomGenerator,
                val gridDims : Dimensions) {

  val startingSnake = Vector(
    new SnakeBodyPart(
      East(),
      new Point(0, 0)
    ),
    new SnakeBodyPart(
      East(),
      new Point(1, 0)
    ),
    new SnakeBodyPart(
      East(),
      new Point(2, 0)
    )
  )
  val startingFrame: GameFrame = new GameFrame(
    dimensionsOfGameCanvas = gridDims,
    snake = startingSnake,
    apple = GameFrame.calculateNewApplePosition(gridDims, random, startingSnake)
  )
  var gameFrames : SStack[GameFrame] = SStack[GameFrame](startingFrame)
  var headDirection : Direction = East()
  def gameOver: Boolean = {
    gameFrames.top.isSnakeHeadTouchingBody()
  }

  def step(): Unit = {
    if (!gameOver){
      gameFrames = gameFrames.push(gameFrames.top.refreshFrame(headDirection, random))
    }
  }

  // TODO implement me
  def setReverse(r: Boolean): Unit = ()

  def changeDir(d: Direction): Unit = {
    if(d != gameFrames.top.getSnakeHead().direction.opposite) headDirection = d
  }

  def getCellType(p : Point): CellType = gameFrames.top.cellTypeAt(p)

}

/** GameLogic companion object */
object GameLogic {

  val FramesPerSecond: Int = 1 // change this to increase/decrease speed of game1

  val DrawSizeFactor = 1.0 // increase this to make the game bigger (for high-res screens)
  // or decrease to make game smaller

  // These are the dimensions used when playing the game.
  // When testing the game, other dimensions are passed to
  // the constructor of GameLogic.
  //
  // DO NOT USE the variable DefaultGridDims in your code!
  //
  // Doing so will cause tests which have different dimensions to FAIL!
  //
  // In your code only use gridDims.width and gridDims.height
  // do NOT use DefaultGridDims.width and DefaultGridDims.height
  val DefaultGridDims
    : Dimensions =
    Dimensions(width = 6, height = 1)  // you can adjust these values to play on a different sized board
}

case class GameFrame(
                 private val dimensionsOfGameCanvas : Dimensions,
                 private val snake : Vector[SnakeBodyPart],
                 private val apple: Point = null,
                 private val growthQueue : Int = 0
                ) {
  def cellTypeAt(p: Point): CellType =
    if (isHead(p))        SnakeHead(snake.last.direction)
    else if (isBody(p))   SnakeBody()
    else if (isApple(p))  Apple()
    else Empty()
  def isBody(p: Point): Boolean = {
    for (aSnakeBodyPart <- snake) {
      if (aSnakeBodyPart.position == p) return true
    }
    return false
  }
  def isHead(p: Point) : Boolean = p == snake.last.position
  def isApple(p: Point) : Boolean = apple == p
  def refreshFrame( newDirection: Direction, randomGenerator: RandomGenerator) : GameFrame = {
    val needsToGrow = growthQueue > 0
    val newSnake = if (needsToGrow) generateNewSnakeWithGrowth (newDirection) else generateNewSnakeWithoutGrowth(newDirection)
    val appleEaten = newSnake.last.position == apple

    val newApple =
      if (appleEaten) GameFrame.calculateNewApplePosition(dimensionsOfGameCanvas, randomGenerator, newSnake)
      else apple

    val newGrowthQueue = {
      if (!needsToGrow && appleEaten)       growthQueue + 3
      else if (needsToGrow && appleEaten)   growthQueue + 2
      else if (needsToGrow && !appleEaten)  growthQueue - 1
      else                                  growthQueue
    }

    val newGameFrame = GameFrame(
      dimensionsOfGameCanvas,
      newSnake,
      newApple,
      newGrowthQueue
    )

    return newGameFrame
  }
  private def generateNewSnakeWithGrowth(newDirection: Direction) : Vector[SnakeBodyPart] = {
    val newSnake = snake :+ SnakeBodyPart(
      newDirection,
      calculateNewBodyPartPosition(snake.last.position, newDirection)
    )
    return newSnake
  }
  private def generateNewSnakeWithoutGrowth(newDirection: Direction) : Vector[SnakeBodyPart] = {

    var newSnake = Vector[SnakeBodyPart]()
    for (i <- 0 until snake.length - 1) {
      val aSnakeBodyPart = SnakeBodyPart(
        snake(i + 1).direction,
        calculateNewBodyPartPosition(snake(i).position, snake(i + 1).direction)
      )
      newSnake = newSnake :+ aSnakeBodyPart
    }

    newSnake = newSnake :+ SnakeBodyPart (
      newDirection,
      calculateNewBodyPartPosition(snake.last.position, newDirection)
    )

    return newSnake
  }
  private def calculateNewBodyPartPosition(oldBodyPartPosition : Point, previousDirection : Direction): Point ={
    val newBodyPartLocation = oldBodyPartPosition + previousDirection.toPoint
    val outOfBoundsLocation: Point = dimensionsOfGameCanvas.getLocationIfPointOutOfBounds(newBodyPartLocation)
    if (outOfBoundsLocation == null) newBodyPartLocation else outOfBoundsLocation
  }
  def isSnakeHeadTouchingBody():Boolean = {
    val snakeBody = snake.init
    val snakeHead = snake.last

    for (aSnakePart <- snakeBody){
      if (snakeHead.position == aSnakePart.position) return true
    }
    return false
  }
  def getSnakeHead():SnakeBodyPart = return snake.last
}

object GameFrame {
  def calculateNewApplePosition(dimensionsOfGameCanvas: Dimensions, randomGenerator: RandomGenerator, snakeBody: Vector[SnakeBodyPart]): Point = {
    val allPoints = dimensionsOfGameCanvas.allPointsInside.diff(SnakeBodyPart.toPoints(snakeBody))

    if (allPoints.size <= 0) {
      return  null
    } else {
      val appleIndex = randomGenerator.randomInt(allPoints.size)
      return allPoints(appleIndex)
    }
  }
}
