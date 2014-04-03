import java.io.{BufferedReader, File, PrintWriter}
import scala.io.Source

object Main {

  def main(args: Array[String]) {
    if (args.length != 1) {
      println("Usage: 'scala Main <inputFile>'")
      System.exit(1)
    }
    val name = args(0)
    val file = Source.fromFile(name)
    val reader = file.bufferedReader()
    val writer = new PrintWriter(new File("output.txt"))
    val t = reader.readLine().head
    val trumpSuite = makeSuite(t)
    proceed(reader, writer, trumpSuite)
    //println("player "+ winner + " won!!!")

  }

  def makeSuite(s: Char) = s match {
    case 'H' | 'D' | 'C' | 'S' => new Suite(s)
    case _ => throw new Exception("unknown suite")
  }

  def makePlayers(s: String): List[Player] = {
    println("s: "+ s)
    val cardStrs = s.split('|').toList.map(_.trim)
    cardStrs.zipWithIndex.map{ si =>
      makePlayer(si._1, si._2 + 1)
    }
  }

  def makePlayer(s: String, number: Int): Player = {
    val cards = s.split(' ').toList.map{ c =>
      val suit = new Suite(c(0))
      val rank = c(1)
      new Card(suit, rank)
    }
    new Player(cards, number)
  }

  def proceed(reader: BufferedReader, writer: PrintWriter, trumpSuite: Suite) {
    val line = reader.readLine()
    //println("line: "+ line)

    line match {
      case "" | null =>
        reader.close()
        writer.close()

      case _ =>
        val players = makePlayers(line)
        val stupid = new Stupid(trumpSuite, players(0), players(1))
        val winner = stupid.play()
        print(winner)
        writer.write(winner)
        proceed(reader, writer, trumpSuite)
    }
  }

}
