package ex2

import ex2.RobotWithBattery.{DEFAULT_CONSUMPTION_RATE, DEFAULT_INITIAL_BATTERY}

import scala.util.Random

type Position = (Int, Int)
enum Direction:
  case North, East, South, West
  def turnRight: Direction = this match
    case Direction.North => Direction.East
    case Direction.East  => Direction.South
    case Direction.South => Direction.West
    case Direction.West  => Direction.North

  def turnLeft: Direction = this match
    case Direction.North => Direction.West
    case Direction.West  => Direction.South
    case Direction.South => Direction.East
    case Direction.East  => Direction.North

trait Robot:
  def position: Position
  def direction: Direction
  def turn(dir: Direction): Unit
  def act(): Unit

class SimpleRobot(var position: Position, var direction: Direction)
    extends Robot:
  def turn(dir: Direction): Unit = direction = dir
  def act(): Unit = position = direction match
    case Direction.North => (position._1, position._2 + 1)
    case Direction.East  => (position._1 + 1, position._2)
    case Direction.South => (position._1, position._2 - 1)
    case Direction.West  => (position._1 - 1, position._2)

  override def toString: String = s"robot at $position facing $direction"

class DumbRobot(val robot: Robot) extends Robot:
  export robot.{position, direction, act}
  override def turn(dir: Direction): Unit = {}
  override def toString: String = s"${robot.toString} (Dump)"

class LoggingRobot(val robot: Robot) extends Robot:
  export robot.{position, direction, turn}
  override def act(): Unit =
    robot.act()
    println(robot.toString)

class RobotWithBattery(
    val robot: Robot,
    val initialBattery: Int = DEFAULT_INITIAL_BATTERY,
    val consumptionRate: Int = DEFAULT_CONSUMPTION_RATE
) extends Robot:
  private var battery = initialBattery
  export robot.{position, direction}
  private def doActionIfCharged(action: => Unit): Unit =
    if battery > 0 then
      battery = battery - consumptionRate
      action
  override def turn(dir: Direction): Unit = doActionIfCharged(robot.turn(dir))
  override def act(): Unit = doActionIfCharged(robot.act())

object RobotWithBattery:
  val DEFAULT_INITIAL_BATTERY = 100
  val DEFAULT_CONSUMPTION_RATE = 25

class RobotCanFail(val robot: Robot, val failureProbability: Double)
    extends Robot:
  require(failureProbability >= 0.0 && failureProbability <= 1.0)
  private val random = Random()
  export robot.{position, direction}
  private def doActionMayFail(action: => Unit): Unit =
    if random.nextDouble() >= failureProbability then action
  override def turn(dir: Direction): Unit = doActionMayFail(robot.turn(dir))
  override def act(): Unit = doActionMayFail(robot.act())

class RobotRepeated(val robot: Robot, val repetitions: Int) extends Robot:
  require(repetitions >= 0)
  export robot.{position, direction}
  private def doMultipleTimes(action: => Unit): Unit =
    1 to repetitions foreach (_ => action)
  override def turn(dir: Direction): Unit = doMultipleTimes(robot.turn(dir))
  override def act(): Unit = doMultipleTimes(robot.act())

@main def testRobot(): Unit =
  val robot = LoggingRobot(SimpleRobot((0, 0), Direction.North))
  robot.act() // robot at (0, 1) facing North
  robot.turn(robot.direction.turnRight) // robot at (0, 1) facing East
  robot.act() // robot at (1, 1) facing East
  robot.act() // robot at (2, 1) facing East
