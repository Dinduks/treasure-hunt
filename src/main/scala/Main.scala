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
    val Stay = Value("S")
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

    start(game, Option(g => drawGame(g)))
  }

  def start(game: Game, drawFunc: Option[Game => Unit] = None): Game = {
    def step(game: Game): Game = {
      drawFunc.map(_.apply(game))

      if (isGameOver(game.players)) {
        val players = game.players.map { player => // we want to ignore the "Stay" moves
          val numberOfStays = player.moves.count(_ == Stay)
          player.copy(movesCounter = player.movesCounter - numberOfStays)
        }
        game.copy(players = players)
      } else {
        val (players, treasures) = computeMoves(game.players, Nil, game.treasures, game)
        step(game.copy(players = players, treasures = treasures))
      }
    }

    step(game)
  }

  def isGameOver(players: Seq[Player]) =
    !players.exists(p => p.movesCounter < p.moves.size)

  def moveForward(orientation: Orientation, position: Position,
    game: Game, otherPlayers: Seq[Player] = Nil): Position = {
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

    val newPosition = Position(newX, newY)

    if (game.mountains.exists(_.position == newPosition) ||
      otherPlayers.exists(_.position == newPosition)) {
      position
    } else {
      newPosition
    }
  }

  def turn(orientation: Orientation, move: Move): Orientation = {
    val o = Seq(North, East, South, West)

    def predecessor(orientation: Orientation) = o((o.indexOf(orientation) + 3) % 4)

    def successor(orientation: Orientation) = o((o.indexOf(orientation) + 1) % 4)

    move match {
      case Right => successor(orientation)
      case Left => predecessor(orientation)
      case Stay => orientation
      case _ => throw new Exception("This kind of move shouldn't be there.") // bhoo
    }
  }

  private def computeMoves(left: Seq[Player], rest: Seq[Player],
    treasures: Seq[Treasure], game: Game): (Seq[Player], Seq[Treasure]) = left match {
    case Nil => (rest, treasures)
    case _ =>
      val player = left.head
      val (newPlayer, gameTreasures) = if (player.movesCounter == player.moves.size) {
        (player, game.treasures)
      } else {
        val (position, orientation) = player.moves(player.movesCounter) match {
          case Forward =>
            val newPosition = moveForward(player.orientation, player.position, game, rest)
            (newPosition, player.orientation)
          case _ =>
            val newOrientation: Orientation = turn(player.orientation, player.moves(player.movesCounter))
            (player.position, newOrientation)
        }

        val (foundTreasure, gameTreasures, playerTreasures) = game.treasures.find(_.position == position) match {
          case Some(treasure) => (true, game.treasures diff Seq(treasure), player.treasuresFound :+ treasure)
          case None => (false, game.treasures, player.treasuresFound)
        }

        val moves: Seq[Move] = if (foundTreasure) {
          val moves = player.moves
          val counter = player.movesCounter
          (moves.slice(0, counter + 1) :+ Stay) ++ moves.slice(counter + 1, moves.size)
        } else if (rest.exists(_.position == position)) {
          val moves = player.moves
          val counter = player.movesCounter
          (moves.slice(0, counter + 1) :+ Stay) ++ moves.slice(counter + 2, moves.size)
        } else {
          player.moves
        }

        val newPlayer = player.copy(position = position,
          orientation = orientation,
          movesCounter = player.movesCounter + 1,
          treasuresFound = playerTreasures,
          moves = moves)

        (newPlayer, gameTreasures)
      }

      computeMoves(left.tail, rest :+ newPlayer, gameTreasures, game.copy(treasures = gameTreasures))
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
