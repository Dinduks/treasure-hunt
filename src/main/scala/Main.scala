object Main {
  def main(args: Array[String]) = {
    val configFile = args.head
    val configuration = scala.io.Source.fromFile(configFile).getLines.mkString("\n")
    val game = parseConfiguration(configuration)
  }

  case class Game(map: Map, treasures: Seq[Treasure], mountains: Seq[Mountain])
  case class Map(width: Long, height: Long)
  case class Position(x: Long, y: Long)
  case class Treasure(position: Position, quantity: Long)
  case class Mountain(position: Position)

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

    Game(map, treasures, mountains)
  }
}
