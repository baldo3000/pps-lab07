package ex2

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class RobotCanFailSpec extends AnyFlatSpec with Matchers:
  "A RobotCanFail" should "never fail when probability is 0.0" in:
    // probability 0.0 => all actions must succeed
    val robot = new RobotCanFail(SimpleRobot((0, 0), Direction.North), 0.0)
    robot.act()
    robot.position should be((0, 1))
    robot.turn(Direction.East)
    robot.direction should be(Direction.East)
    robot.act()
    robot.position should be((1, 1))

  it should "always fail when probability is 1.0" in:
    // probability 1.0 => all actions must be skipped (no state change)
    val robot = new RobotCanFail(SimpleRobot((0, 0), Direction.North), 1.0)
    robot.act()
    // no movement should have occurred
    robot.position should be((0, 0))
    robot.turn(Direction.East)
    // turn should be skipped
    robot.direction should be(Direction.North)
    // additional act still skipped
    robot.act()
    robot.position should be((0, 0))
