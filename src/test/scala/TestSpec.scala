
import org.scalatest._
import Main._
import Main.Orientation._
import Main.Move._

class TestSpec extends WordSpec with Matchers {
  "parseConfiguration" should {
    lazy val configuration =
      """
        |C 6 5
        |T 4-2 1
        |T 1-4 3
        |M 5-3
        |John 1-1 E AADADAGA Bunny
      """.stripMargin

    "return a map" in {
      val game = parseConfiguration(configuration)
      game.map.width shouldBe 6
      game.map.height shouldBe 5
    }

    "return a list of treasures" in {
      val game = parseConfiguration(configuration)
      game.treasures.size shouldBe 2
      game.treasures.head.position.x shouldBe 4
      game.treasures.head.position.y shouldBe 2
      game.treasures.head.quantity shouldBe 1
      game.treasures(1).position.x shouldBe 1
      game.treasures(1).position.y shouldBe 4
      game.treasures(1).quantity shouldBe 3
    }

    "return a list of mountains" in {
      val game = parseConfiguration(configuration)
      game.mountains.size shouldBe 1
      game.mountains.head.position.x shouldBe 5
      game.mountains.head.position.y shouldBe 3
    }

    "return a list of players" in {
      val game = parseConfiguration(configuration)
      game.players.size shouldBe 1
      game.players.head.name shouldBe "Bunny"
      game.players.head.position.x shouldBe 1
      game.players.head.position.y shouldBe 1
      game.players.head.orientation shouldBe East
      game.players.head.moves shouldBe List(Forward, Forward, Right, Forward, Right, Forward, Left, Forward)
      game.players.head.movesCounter shouldBe 0
    }
  }
}
