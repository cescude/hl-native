import java.io.{BufferedReader, InputStreamReader}

import org.rogach.scallop.ScallopConf

import scala.util.matching.Regex

object Main {

  class Config(args: Seq[String]) extends ScallopConf(args) {
    banner("""
      |Reads the contents STDIN and colorizes the matching regexp. Essentially,
      |this is like running a highlighter through your console.
      |""".stripMargin)

    val ignoreCase = opt[Boolean]("ignore-case", short='i')
    val tokens = trailArg[List[String]]("exprs", descr="List of regular expressions to match")
    verify()
  }

  val stdin: Iterator[String] = {
    val reader = new BufferedReader( new InputStreamReader( System.in ) )
    Iterator.continually(reader.readLine).takeWhile(_ != null)
  }

  val colors: Vector[fansi.Attr] = {
    import fansi.Color._
    Vector(Red, Green, Blue, Yellow, Magenta, Cyan)
  }

  def main(args: Array[String]): Unit = {

    val conf = new Config(args)
    val rxs: Seq[(Regex,fansi.Attr)] = conf.tokens()
      .map(s => if (conf.ignoreCase()) s"(?i)$s" else s)
      .zip(Stream.from(0))
      .map({
        case (s, idx) => (s.r, colors(idx % colors.length))
      })

    stdin.foreach({ rawLine =>
      val hl: fansi.Str = rxs.foldLeft(fansi.Str(rawLine)) {
        case (str, (rx, c)) =>
          rx.findAllMatchIn(rawLine)
            .map(m => (m.start, m.end))
            .foldLeft(str) {
              case (s, (start, end)) => s.overlay(c, start, end).overlay(fansi.Reversed.On, start, end)
            }
      }

      println(hl)
    })
  }
}
