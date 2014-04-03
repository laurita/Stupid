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

    //println("\npercentage: "+ percentageTruth())

  }

  def makeSuite(s: Char) = s match {
    case 'H' | 'D' | 'C' | 'S' => new Suite(s)
    case _ => throw new Exception("unknown suite")
  }

  def makePlayers(s: String): List[Player] = {
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

    line match {
      case "" | null =>
        reader.close()
        writer.close()

      case _ =>
        val players = makePlayers(line)
        val stupid = new Stupid(trumpSuite, players(0), players(1))
        val winner = stupid.play()
        print(winner)
        writer.write(winner.toString)
        proceed(reader, writer, trumpSuite)
    }
  }

  def percentageTruth(): Double = {
    val file1 = Source.fromFile("data/results.txt")
    val reader1 = file1.bufferedReader()
    val exp = reader1.readLine()
    val file2 = Source.fromFile("output.txt")
    val reader2 = file2.bufferedReader()
    val act = reader2.readLine()
    exp.toList.map(_.asDigit).zip(act.toList.map(_.asDigit)).foldLeft(0)( (b, a) => {
      if (a._1 == a._2) b+1 else b
    } ).toDouble / exp.length
  }

}
