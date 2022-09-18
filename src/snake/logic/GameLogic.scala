package snake.logic

import engine.random.{RandomGenerator, ScalaRandomGen}
import snake.logic.GameLogic._

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
    snake = Array(
      new SnakeBodyPart(
        East(),
        new Point(2, 0)
      ),
      new SnakeBodyPart(
        East(),
        new Point(1, 0)
      ),
      new SnakeBodyPart(
        East(),
        new Point(0, 0)
      )
    )
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
    headDirection = d
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
                 private val snake : Array[SnakeBodyPart],
                 private var apple: Point = null,
                 private var growthQueue : Queue[SnakeBodyPart] = Queue(),
                 private val grow : Boolean = false
               ) {
  def cellTypeAt(p: Point): CellType =
    if (isHead(p))        SnakeHead(snake(0).direction)
    else if (isBody(p))   SnakeBody()
    else if (isApple(p))  Apple()
    else Empty()
  def isHead(p: Point) : Boolean = p == snake(0).position
  def isBody(p: Point) : Boolean = {
    for ( aSnakeBodyPart <- snake){
      if (aSnakeBodyPart.position == p) return true
    }
    return false
  }
  def isApple(p: Point) : Boolean = apple == p
  def refreshFrame( newDirection: Direction, randomGenerator: RandomGenerator) : GameFrame = {

    updateGrowthQueueIfNeeded(randomGenerator)

    val newGameFrame = GameFrame(
      dimensionsOfGameCanvas,
      generateNewSnake(newDirection),
      apple,
      growthQueue
    )
    return newGameFrame
  }
  private def generateNewSnake(newDirection: Direction) : Array[SnakeBodyPart] = {
    var readyToGrow = !growthQueue.isEmpty && growthQueue.dequeue._1.position == snake.last.position
    var lengthOfSnake = if (readyToGrow) snake.length + 1 else snake.length
    val newSnake = new Array[SnakeBodyPart](lengthOfSnake)
    newSnake(0) = SnakeBodyPart(
      newDirection,
      calculateNewBodyPartPosition(snake(0).position, newDirection)
    )

    for (i <- 1 until snake.length){
      newSnake(i) = SnakeBodyPart(
        snake(i - 1).direction,
        calculateNewBodyPartPosition(snake(i).position, snake(i - 1).direction)
      )
    }
    if(readyToGrow){
      newSnake(snake.length) = growthQueue.dequeue._1
      growthQueue = growthQueue.dequeue._2
    }
    return newSnake
  }
  private def calculateNewBodyPartPosition(oldBodyPartPosition : Point, previousDirection : Direction): Point ={
    val newBodyPartLocation = oldBodyPartPosition + previousDirection.toPoint
    val outOfBoundsLocation: Point = dimensionsOfGameCanvas.getLocationIfPointOutOfBounds(newBodyPartLocation)
    if (outOfBoundsLocation == null) newBodyPartLocation else outOfBoundsLocation
  }
  def calculateNewApplePosition(randomGenerator: RandomGenerator) :Unit = {
    var freeSpotsIndex : Int = 0
    var freeSpotsPositions : Array[Point] = new Array[Point](dimensionsOfGameCanvas.height*dimensionsOfGameCanvas.width)
    for( aPoint <- dimensionsOfGameCanvas.allPointsInside){
      if (!isBody(aPoint)){
        freeSpotsPositions(freeSpotsIndex) = aPoint
        freeSpotsIndex += 1
      }
    }

    var randomFreePoint = randomGenerator.randomInt(freeSpotsIndex)
    apple = freeSpotsPositions(randomFreePoint)
  }

  private def updateGrowthQueueIfNeeded(randomGenerator: RandomGenerator) : Unit ={

    if (snake(0).position != apple) return ()
    calculateNewApplePosition(randomGenerator)
    for (i <- 0 until 3) {
      growthQueue = growthQueue.enqueue(
        new SnakeBodyPart(
          snake(0).direction,
          snake(0).position
        )
      )
    }
  }
}


