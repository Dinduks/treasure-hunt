
import org.scalatest._
import Main._
import Main.Orientation._
import Main.Move._

class TestSpec extends WordSpec with Matchers {
  /**
   * M: mountain — X: Treasure
   *
   *   → → ↓ . . .
   *   . M ↓ . . .
   *   . 1 2 . . .
   *   . . . . . .
   *   . . . . . M
   *
   */
  lazy val configuration =
    """
      |C 6 5
      |T 2-3 1
      |T 3-3 2
      |M 2-2
      |M 6-5
      |John 1-1 E AADADAGA Bunny
    """.stripMargin

  "parseConfiguration" should {
    "return a map" in {
      val game = parseConfiguration(configuration)
      game.map.width shouldBe 6
      game.map.height shouldBe 5
    }

    "return a list of treasures" in {
      val game = parseConfiguration(configuration)
      game.treasures.size shouldBe 2
      game.treasures.head.position.x shouldBe 1
      game.treasures.head.position.y shouldBe 2
      game.treasures.head.quantity shouldBe 1
      game.treasures(1).position.x shouldBe 2
      game.treasures(1).position.y shouldBe 2
      game.treasures(1).quantity shouldBe 2
    }

    "return a list of mountains" in {
      val game = parseConfiguration(configuration)
      game.mountains.size shouldBe 2
      game.mountains.head.position.x shouldBe 1
      game.mountains.head.position.y shouldBe 1
      game.mountains(1).position.x shouldBe 5
      game.mountains(1).position.y shouldBe 4
    }

    "return a list of players" in {
      val game = parseConfiguration(configuration)
      game.players.size shouldBe 1
      game.players.head.name shouldBe "Bunny"
      game.players.head.position.x shouldBe 0
      game.players.head.position.y shouldBe 0
      game.players.head.orientation shouldBe East
      game.players.head.moves shouldBe List(Forward, Forward, Right, Forward, Right, Forward, Left, Forward)
      game.players.head.movesCounter shouldBe 0
      game.players.head.treasuresFound shouldBe Nil
    }
  }

  "start" should {
    "return a new players' list" in {
      val game = start(parseConfiguration(configuration))
      game.players.size shouldBe 1
      game.players.head.position.x shouldBe 2
      game.players.head.position.y shouldBe 2
      game.players.head.orientation shouldBe South
      game.players.head.movesCounter shouldBe 8
      game.players.head.treasuresFound.size shouldBe 1
      game.players.head.treasuresFound.head.quantity shouldBe 2
    }
  }

  "moveFoward" should {
    lazy val game = parseConfiguration(configuration)

    "move to the north" in {
      val position = moveForward(North, Position(3, 2), game)
      position.x shouldBe 3
      position.y shouldBe 1
    }

    "move to the east" in {
      val position = moveForward(East, Position(4, 3), game)
      position.x shouldBe 5
      position.y shouldBe 3
    }

    "move to the south" in {
      val position = moveForward(South, Position(3, 2), game)
      position.x shouldBe 3
      position.y shouldBe 3
    }

    "move to the west" in {
      val position = moveForward(West, Position(3, 2), game)
      position.x shouldBe 2
      position.y shouldBe 2
    }

    "don't move outside of the map" in {
      {
        val position = moveForward(North, Position(0, -1000000), game)
        position.x shouldBe 0
        position.y shouldBe 0
      }

      {
        val position = moveForward(North, Position(-1000000, 0), game)
        position.x shouldBe 0
        position.y shouldBe 0
      }

      {
        val position = moveForward(North, Position(0, 1000000), game)
        position.x shouldBe 0
        position.y shouldBe game.map.height - 1
      }

      {
        val position = moveForward(North, Position(1000000, 0), game)
        position.x shouldBe game.map.width - 1
        position.y shouldBe 0
      }
    }

    "don't move where a mountain is" in {
      val position = moveForward(South, Position(5, 3), game)
      position.x shouldBe 5
      position.y shouldBe 3
    }
  }

  "turn" should {
    "turn to the left" in {
      turn(North, Left) shouldBe West
      turn(East,  Left) shouldBe North
      turn(South, Left) shouldBe East
      turn(West,  Left) shouldBe South
    }

    "turn to the right" in {
      turn(North, Right) shouldBe East
      turn(East,  Right) shouldBe South
      turn(South, Right) shouldBe West
      turn(West,  Right) shouldBe North
    }
  }
}
