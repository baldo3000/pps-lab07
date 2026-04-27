package ex2

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class DumbRobotSpec extends AnyFlatSpec with Matchers:
  "A Dumb Robot" should "never turn" in:
    val robot = DumbRobot(SimpleRobot((0, 0), Direction.North))

    robot.turn(Direction.East)
    robot.direction should be(Direction.North)
