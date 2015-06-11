object Main {
  object Orientation extends Enumeration {
    type Orientation = Value
    val North = Value("N")
    val East  = Value("E")
    val South = Value("S")
    val West  = Value("O")
  }
  import Orientation._

  object Move extends Enumeration {
    type Move = Value
    val Forward = Value("A")
    val Right   = Value("D")
    val Left    = Value("G")
  }
  import Move._

  case class Game(map: Map, treasures: Seq[Treasure], mountains: Seq[Mountain], players: Seq[Player])
  case class Map(width: Int, height: Int)
  case class Position(x: Long, y: Long)
  case class Treasure(position: Position, quantity: Long)
  case class Mountain(position: Position)
  case class Player(name: String, position: Position, orientation: Orientation, moves: List[Move],
    movesCounter: Int, treasures: Long)

  def main(args: Array[String]): Unit = {
    val configFile = args.head
    val configuration = scala.io.Source.fromFile(configFile).getLines.mkString("\n")
    val game = parseConfiguration(configuration)

    start(game)
  }

  def moveForward(orientation: Orientation, position: Position, game: Game): Position = {
    val (x, y) = orientation match {
      case Orientation.North => (position.x, position.y - 1)
      case Orientation.East => (position.x + 1, position.y)
      case Orientation.South => (position.x, position.y + 1)
      case Orientation.West => (position.x - 1, position.y)
    }

    val newX = if (x < 0) 0
    else if (x > game.map.width - 1) game.map.width - 1
    else x

    val newY = if (y < 0) 0
    else if (y > game.map.height - 1) game.map.height - 1
    else y

    game.mountains.find(_.position == Position(newX, newY))
      .map(_ => position)
      .getOrElse(Position(newX, newY))
  }

  def turn(orientation: Orientation, move: Move): Orientation = {
    val o = Seq(North, East, South, West)

    def predecessor(orientation: Orientation) = {
      o((o.indexOf(orientation) + 3) % 4)
    }

    def successor(orientation: Orientation) = {
      o((o.indexOf(orientation) + 1) % 4)
    }

    move match {
      case Right => successor(orientation)
      case Left  => predecessor(orientation)
      case _     => throw new Exception("This kind of move shouldn't be there.") // bhoo
    }
  }

  def start(game: Game): Game = {
    def ƒ(game: Game): Game = {
      val player = game.players.head
      if (player.movesCounter == player.moves.size) {
        game
      } else {
        val (position, orientation) =
          if (player.moves(player.movesCounter) == Move.Forward) {
            val newPosition = moveForward(player.orientation, player.position, game)
            val newOrientation = player.orientation
            (newPosition, newOrientation)
          } else {
            val newPosition = player.position
            val newOrientation: Orientation = turn(player.orientation, player.moves(player.movesCounter))
            (newPosition, newOrientation)
          }

        val newPlayer = player.copy(position = position,
          orientation = orientation,
          movesCounter = player.movesCounter + 1)

        ƒ(game.copy(players = List(newPlayer)))
      }
    }

    ƒ(game)
  }

  def parseConfiguration(config: String): Game = {
    val splitConfig = config.split("\n")

    val mapLine = splitConfig.filter(_.startsWith("C ")).head
    val map = Map(width = mapLine.split(" ")(1).toInt, height = mapLine.split(" ")(2).toInt)

    val treasures: Seq[Treasure] = splitConfig.filter(_.startsWith("T ")).map { line =>
      val splitLine = line.split(" ")
      val position = Position(x = splitLine(1).split("-")(0).toLong - 1,
        y = splitLine(1).split("-")(1).toLong - 1)
      Treasure(position, splitLine(2).toLong)
    }

    val mountains: Seq[Mountain] = splitConfig.filter(_.startsWith("M ")).map { line =>
      val splitLine = line.split(" ")
      val position = Position(x = splitLine(1).split("-")(0).toLong - 1,
        y = splitLine(1).split("-")(1).toLong - 1)
      Mountain(position)
    }

    val players: Seq[Player] = splitConfig
      .filter(_.trim.nonEmpty)
      .filter(l => !l.startsWith("C ") && !l.startsWith("T ") && !l.startsWith("M "))
      .map { line =>
        val splitLine = line.split(" ")
        val name = splitLine(4)
        val position = Position(x = splitLine(1).split("-")(0).toLong - 1,
          y = splitLine(1).split("-")(1).toLong - 1)
        val orientation = Orientation.withName(splitLine(2))
        val moves: List[Move] = splitLine(3).map(c => Move.withName(c.toString)).toList
        Player(name, position, orientation, moves, movesCounter = 0, treasures = 0)
      }.toList

    Game(map, treasures, mountains, players)
  }
}
