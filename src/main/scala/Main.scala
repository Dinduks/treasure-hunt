object Main {
  def main(args: Array[String]) = {
    val configFile = args.head
    val configuration = scala.io.Source.fromFile(configFile).getLines.mkString("\n")
    val game = parseConfiguration(configuration)
  }

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
    val Right = Value("D")
    val Left = Value("G")
  }
  import Move._

  case class Game(map: Map, treasures: Seq[Treasure], mountains: Seq[Mountain], players: Seq[Player])
  case class Map(width: Long, height: Long)
  case class Position(x: Long, y: Long)
  case class Treasure(position: Position, quantity: Long)
  case class Mountain(position: Position)
  case class Player(name: String, position: Position, orientation: Orientation, moves: List[Move], movesCounter: Long)

  def parseConfiguration(config: String): Game = {
    val splitConfig = config.split("\n")

    val mapLine = splitConfig.filter(_.startsWith("C ")).head
    val map = Map(width = mapLine.split(" ")(1).toLong, height = mapLine.split(" ")(2).toLong)

    val treasures: Seq[Treasure] = splitConfig.filter(_.startsWith("T ")).map { line =>
      val splitLine = line.split(" ")
      val position = Position(x = splitLine(1).split("-")(0).toLong,
        y = splitLine(1).split("-")(1).toLong)
      Treasure(position, splitLine(2).toLong)
    }

    val mountains: Seq[Mountain] = splitConfig.filter(_.startsWith("M ")).map { line =>
      val splitLine = line.split(" ")
      val position = Position(x = splitLine(1).split("-")(0).toLong,
        y = splitLine(1).split("-")(1).toLong)
      Mountain(position)
    }

    val players: Seq[Player] = splitConfig
      .filter(_.trim.nonEmpty)
      .filter(l => !l.startsWith("C ") && !l.startsWith("T ") && !l.startsWith("M "))
      .map { line =>
        val splitLine = line.split(" ")
        val name = splitLine(4)
        val position = Position(x = splitLine(1).split("-")(0).toLong,
          y = splitLine(1).split("-")(1).toLong)
        val orientation = Orientation.withName(splitLine(2))
        val moves: List[Move] = splitLine(3).map(c => Move.withName(c.toString)).toList
        Player(name, position, orientation, moves, movesCounter = 0)
      }.toList

    Game(map, treasures, mountains, players)
  }
}
