package ya.divaac

import org.specs.Specification
import org.specs.util.HmsTimer
import org.scalacheck.Gen

class EscapeSpecs extends Specification with HmsTimer {

  private val escapeMap = Map(
    '\r' -> 'r', '\n' -> 'n', '\t' -> 't', '\f' -> 'f',
    '\\' -> '\\', '\'' -> '\'', '\"' -> '\"', '\b' -> 'b')
  def escape(content: String): String = {
    content.foldLeft(new StringBuffer){(buf, ch) =>
      ch match {
        case ch if escapeMap.contains(ch) =>
          buf.append("\\"); buf.append(escapeMap(ch))
        case ch => buf.append(ch)
      }
    }.toString
  }
  def escape1(content: String): String = {
    content.foldLeft(new StringBuilder){(buf, ch) =>
      ch match {
        case ch if escapeMap.contains(ch) =>
          buf.append("\\"); buf.append(escapeMap(ch))
        case ch => buf.append(ch)
      }
    }.toString
  }
  def escape15(content: String): String = {
    content.foldLeft(new java.lang.StringBuilder){(buf, ch) =>
      ch match {
        case ch if escapeMap.contains(ch) =>
          buf.append("\\"); buf.append(escapeMap(ch))
        case ch => buf.append(ch)
      }
    }.toString
  }
  def escape2(content: String): String =
    content.flatMap{case ch if escapeMap.contains(ch) => "\\" + escapeMap(ch); case ch => ch.toString}


  def benchmark(esc: String => String, samples: Seq[String]) = {
    start
    for (sample <- samples) {
      esc(sample)
    }
    stop
    printf("%d times escaped. elapsed %d ms\n", samples.size, elapsed)
  }

  "benchmark" should {
    skip("fastest j.l.StringBuilder")
    "escape j.l.StringBuffer" in { benchmark(escape, EscapeSpecs.samples) }
    "escape1 s.StringBuilder" in { benchmark(escape1, EscapeSpecs.samples) }
    "escape15 j.l.StringBuilder" in { benchmark(escape15, EscapeSpecs.samples) }
    "escape2" in { benchmark(escape2, EscapeSpecs.samples) }
  }
}

object EscapeSpecs {
  val g = Gen.alphaStr
  val samples = (1 to 50000) map(_ => g.sample.get) toSeq;
}
