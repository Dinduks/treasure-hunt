object Main {
  object Orientation extends Enumeration {
    type Orientation = Value
    val North = Value("N")
    val East = Value("E")
    val South = Value("S")
    val West = Value("O")
  }
  import Orientation._

  object Move extends Enumeration {
    type Move = Value
    val Forward = Value("A")
    val Right = Value("D")
    val Left = Value("G")
  }
  import Move._

  case class Game(map: Map, treasures: Seq[Treasure], mountains: Seq[Mountain], players: Seq[Player])
  case class Map(width: Int, height: Int)
  case class Position(x: Long, y: Long)
  case class Treasure(position: Position, quantity: Long)
  case class Mountain(position: Position)
  case class Player(name: String, position: Position, orientation: Orientation,
    moves: Seq[Move], movesCounter: Int, treasuresFound: Seq[Treasure])

  def main(args: Array[String]): Unit = {
    val mapConfig = scala.io.Source.fromFile(args(0)).getLines.mkString("\n")
    val playersConfig = scala.io.Source.fromFile(args(1)).getLines.mkString("\n")
    val game = parseConfiguration(mapConfig, playersConfig)

    start(game, draw = true)
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
      case Left => predecessor(orientation)
      case _ => throw new Exception("This kind of move shouldn't be there.") // bhoo
    }
  }

  def isGameOver(players: Seq[Player]) =
    !players.exists(p => p.movesCounter < p.moves.size)

  def start(game: Game, draw: Boolean = false): Game = {
    def ƒ(game: Game): Game = {
      if (draw) drawGame(game)

      if (isGameOver(game.players)) {
        game
      } else {
        def g(left: Seq[Player], rest: Seq[Player], treasures: Seq[Treasure]): (Seq[Player], Seq[Treasure]) = {
          left match {
            case Nil => (rest, treasures)
            case _ =>
              val player = left.head
              val (newPlayer, gameTreasures) = if (player.movesCounter == player.moves.size) {
                (player, game.treasures)
              } else {
                val (position, orientation) = if (player.moves(player.movesCounter) == Move.Forward) {
                  val newPosition = moveForward(player.orientation, player.position, game)
                  val newOrientation = player.orientation
                  (newPosition, newOrientation)
                } else {
                  val newPosition = player.position
                  val newOrientation: Orientation = turn(player.orientation, player.moves(player.movesCounter))
                  (newPosition, newOrientation)
                }

                val (gameTreasures, playerTreasures) = game.treasures.find(_.position == position) match {
                  case Some(treasure) => (game.treasures diff Seq(treasure), player.treasuresFound :+ treasure)
                  case None => (game.treasures, player.treasuresFound)
                }

                val newPlayer = player.copy(position = position,
                  orientation = orientation,
                  movesCounter = player.movesCounter + 1,
                  treasuresFound = playerTreasures)

                (newPlayer, gameTreasures)
              }

              g(left.tail, rest :+ newPlayer, gameTreasures)
          }
        }

        val (players, treasures) = g(game.players, Nil, game.treasures)

        ƒ(game.copy(players = players, treasures = treasures))
      }
    }

    ƒ(game)
  }

  def drawGame(game: Game, delay: Int = 500): Unit = {
    println("\u001b[2J\u001b[1;1H")
    println(buildDrawableGame(game))
    Thread.sleep(delay)
  }

  def buildDrawableGame(game: Game): String = {
    val sb = new StringBuilder
    for (j <- 0 to game.map.height - 1) {
      for (i <- 0 to game.map.width - 1) {
        if (game.mountains.exists(_.position == Position(i, j))) {
          sb.append(" M")
        } else if (game.treasures.exists(_.position == Position(i, j))) {
          sb.append(s" ${game.treasures.filter(_.position == Position(i, j)).head.quantity}")
        } else if (game.players.exists(_.position == Position(i, j))) {
          val player = game.players.find(_.position == Position(i, j)).head
          player.orientation match {
            case North => sb.append(" ↑")
            case East => sb.append(" →")
            case South => sb.append(" ↓")
            case West => sb.append(" ←")
          }
        } else {
          sb.append(" .")
        }
      }
      sb.append('\n')
    }
    sb.toString()
  }

  def parseConfiguration(mapConfig: String, playersConfig: String) = {
    val (map, treasures, mountains) = parseMapConfiguration(mapConfig)
    val players = parsePlayersConfiguration(playersConfig)
    Game(map, treasures, mountains, players)
  }

  def parseMapConfiguration(config: String): (Map, Seq[Treasure], Seq[Mountain]) = {
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

    (map, treasures, mountains)
  }

  def parsePlayersConfiguration(config: String): Seq[Player] = config.split("\n")
    .filter(_.trim.nonEmpty)
    .filter(l => !l.startsWith("C ") && !l.startsWith("T ") && !l.startsWith("M "))
    .map { line =>
      val splitLine = line.split(" ")
      val name = splitLine(4)
      val position = Position(x = splitLine(1).split("-")(0).toLong - 1,
        y = splitLine(1).split("-")(1).toLong - 1)
      val orientation = Orientation.withName(splitLine(2))
      val moves: Seq[Move] = splitLine(3).map(c => Move.withName(c.toString)).toList
      Player(name, position, orientation, moves, movesCounter = 0, treasuresFound = Nil)
    }.toList
}
