package ex2

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class RobotRepeatedSpec extends AnyFlatSpec with Matchers:
  "A RobotRepeated" should "repeat act N times" in:
    val robot =
      new RobotRepeated(SimpleRobot((0, 0), Direction.North), repetitions = 3)
    robot.act()
    // 3 acts north -> y increased by 3
    robot.position should be((0, 3))

  it should "apply repetition to both act and turn" in:
    // rep = 2: first act -> (0,2); turn to East (repeated) -> direction East; act -> (2,2)
    val robot =
      new RobotRepeated(SimpleRobot((0, 0), Direction.North), repetitions = 2)
    robot.act()
    robot.position should be((0, 2))
    robot.turn(Direction.East)
    robot.direction should be(Direction.East)
    robot.act()
    robot.position should be((2, 2))

  it should "perform no actions when repetitions is 0" in:
    val robot =
      new RobotRepeated(SimpleRobot((0, 0), Direction.North), repetitions = 0)
    robot.act()
    robot.position should be((0, 0))
    robot.turn(Direction.East)
    robot.direction should be(Direction.North)
    // another act still no-op
    robot.act()
    robot.position should be((0, 0))
