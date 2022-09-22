package snake.logic
import engine.random.{RandomGenerator, ScalaRandomGen}

import scala.collection.immutable.Queue

//import java.util.random.RandomGenerator

/** To implement Snake, complete the ``TODOs`` below.
 *
 * If you need additional files,
 * please also put them in the ``snake`` package.
 */

class GameLogic(val random: RandomGenerator,
                val gridDims : Dimensions) {


  val startingFrame: GameFrame = new GameFrame(
    dimensionsOfGameCanvas = gridDims,
    snake = Vector(
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
    )//, apple = new Point(5, 0)
  )
  startingFrame.calculateNewApplePosition(random)
  var gameFrames : SStack[GameFrame] = SStack[GameFrame](startingFrame)
  var headDirection : Direction = East()
  def gameOver: Boolean = false

  // TODO implement me
  def step(): Unit = {

    gameFrames = gameFrames.push(gameFrames.top.refreshFrame(headDirection, random))
  }

  // TODO implement me
  def setReverse(r: Boolean): Unit = ()

  // TODO implement me
  def changeDir(d: Direction): Unit = {
    if(d != headDirection.opposite) headDirection = d
  }

  // TODO implement me
  def getCellType(p : Point): CellType = gameFrames.top.cellTypeAt(p)

}

/** GameLogic companion object */
object GameLogic {

  val FramesPerSecond: Int = 5 // change this to increase/decrease speed of game

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
    Dimensions(width = 25, height = 25)  // you can adjust these values to play on a different sized board
}

case class GameFrame(
                 private val dimensionsOfGameCanvas : Dimensions,
                 private val snake : Vector[SnakeBodyPart],
                 private var apple: Point = null,
                 private val growthQueue : Int = 0
                ) {
  def cellTypeAt(p: Point): CellType =
    if (isHead(p))        SnakeHead(snake.last.direction)
    else if (isBody(p, snake))   SnakeBody()
    else if (isApple(p))  Apple()
    else Empty()
  def isHead(p: Point) : Boolean = p == snake.last.position
  def isBody(p: Point, snakeBody: Vector[SnakeBodyPart]) : Boolean = {
    for ( aSnakeBodyPart <- snakeBody){
      if (aSnakeBodyPart.position == p) return true
    }
    return false
  }
  def isApple(p: Point) : Boolean = apple == p
  def refreshFrame( newDirection: Direction, randomGenerator: RandomGenerator) : GameFrame = {
    var newGrowthQueue = growthQueue
    var newSnake = Vector[SnakeBodyPart]()
    if (growthQueue > 0) {
      newSnake = generateNewSnakeWithGrowth (newDirection)
      newGrowthQueue -= 1
    }
    else {
      newSnake = generateNewSnakeWithoutGrowth(newDirection)
    }

    if (newSnake.last.position == apple) {
      calculateNewApplePosition(randomGenerator, newSnake)
      newGrowthQueue += 3
    }

    val newGameFrame = GameFrame(
      dimensionsOfGameCanvas,
      newSnake,
      apple,
      newGrowthQueue
    )

    return newGameFrame
  }
  private def generateNewSnakeWithGrowth(newDirection: Direction) : Vector[SnakeBodyPart] = {
    var newSnake = Vector[SnakeBodyPart]()
    for (i <- 0 until snake.length){
      newSnake = newSnake :+ SnakeBodyPart(
        snake(i).direction,
        snake(i).position
      )
    }

    newSnake = newSnake :+ SnakeBodyPart(
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
  def calculateNewApplePosition(randomGenerator: RandomGenerator, snakeBody:Vector[SnakeBodyPart] = snake) :Unit = {
    /*var freeSpotsIndex : Int = - 1
    var freeSpotsPositions = Vector[Point]()
    for( aPoint <- dimensionsOfGameCanvas.allPointsInside){
      if (!isBody(aPoint, snakeBody)){
        freeSpotsPositions :+ Point(aPoint.x, aPoint.y)
        freeSpotsIndex += 1
      }
    }

    var randomFreePoint = randomGenerator.randomInt(freeSpotsIndex)
    apple = freeSpotsPositions(randomFreePoint)*/

    val nrFreeSpots = dimensionsOfGameCanvas.allPointsInside.length - snake.length
    val appleIndex = randomGenerator.randomInt(nrFreeSpots)
    apple = dimensionsOfGameCanvas.allPointsInside(appleIndex)
  }

}


