
import org.scalatest._
import Main._
import Main.Orientation._
import Main.Move._

class TestSpec extends WordSpec with Matchers {
  /**
   * M: mountain — X: Treasure
   *
   *   . . . . . .
   *   . M . . . .
   *   . 1 2 . . .
   *   . . . . . .
   *   . . . . . M
   *
   */
  lazy val mapConfig =
    """
      |C 6 5
      |T 2-3 1
      |T 3-3 2
      |M 2-2
      |M 6-5
    """.stripMargin
  
  lazy val playersConfig =
    """
      |John 1-1 E AADADAGA Bunny
    """.stripMargin

  "parseMapConfiguration" should {
    "return a map" in {
      val (map, _, _) = parseMapConfiguration(mapConfig)
      map.width shouldBe 6
      map.height shouldBe 5
    }

    "return a list of treasures" in {
      val (_, treasures, _) = parseMapConfiguration(mapConfig)
      treasures.size shouldBe 2
      treasures.head.position.x shouldBe 1
      treasures.head.position.y shouldBe 2
      treasures.head.quantity shouldBe 1
      treasures(1).position.x shouldBe 2
      treasures(1).position.y shouldBe 2
      treasures(1).quantity shouldBe 2
    }

    "return a list of mountains" in {
      val (_, _, mountains) = parseMapConfiguration(mapConfig)
      mountains.size shouldBe 2
      mountains.head.position.x shouldBe 1
      mountains.head.position.y shouldBe 1
      mountains(1).position.x shouldBe 5
      mountains(1).position.y shouldBe 4
    }
  }

  "parsePlayersConfiguration" should {
    "return a list of players" in {
      val players = parsePlayersConfiguration(playersConfig)
      players.size shouldBe 1
      players.head.name shouldBe "Bunny"
      players.head.position.x shouldBe 0
      players.head.position.y shouldBe 0
      players.head.orientation shouldBe East
      players.head.moves shouldBe List(Forward, Forward, Right, Forward, Right, Forward, Left, Forward)
      players.head.movesCounter shouldBe 0
      players.head.treasuresFound shouldBe Nil
    }
  }

  "start" should {
    "return a new players' list" in {
      val game = start(parseConfiguration(mapConfig, playersConfig))
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
    lazy val game = parseConfiguration(mapConfig, playersConfig)

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
