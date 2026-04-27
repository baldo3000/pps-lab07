package ex2

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class RobotWithBatterySpec extends AnyFlatSpec with Matchers:
  "A RobotWithBattery" should "stop acting when battery is exhausted" in:
    // with initial 50 and consumption 25, two acts are allowed; third is a no-op
    val robot = new RobotWithBattery(
      SimpleRobot((0, 0), Direction.North),
      initialBattery = 50,
      consumptionRate = 25
    )
    robot.act()
    robot.position should be((0, 1))
    robot.act()
    robot.position should be((0, 2))
    // battery is now 0 -> further act should not change position
    robot.act()
    robot.position should be((0, 2))

  it should "stop turning when battery is exhausted" in:
    // with initial 25 and consumption 25, one turn is allowed; second is a no-op
    val robot = new RobotWithBattery(
      SimpleRobot((0, 0), Direction.North),
      initialBattery = 25,
      consumptionRate = 25
    )
    robot.turn(Direction.East)
    robot.direction should be(Direction.East)
    // battery now 0 -> next turn should not change direction
    robot.turn(Direction.South)
    robot.direction should be(Direction.East)

  it should "mix actions and turns consuming battery" in:
    // With initial 30 and consumption 10:
    // act -> battery 20, turn -> battery 10, act -> battery 0 (allowed), next act -> no-op
    val robot = new RobotWithBattery(
      SimpleRobot((0, 0), Direction.North),
      initialBattery = 30,
      consumptionRate = 10
    )
    robot.act() // (0,1)
    robot.turn(Direction.East) // direction East
    robot.act() // (1,1) battery becomes 0 here
    // further act should be a no-op
    robot.act()
    robot.position should be((1, 1))
    robot.direction should be(Direction.East)
