package ya.divaac

import java.net.URL
import scala.io.Source
import scala.xml._
import scala.xml.parsing.XhtmlParser
import java.text.SimpleDateFormat

object DivaacRank {

  case class Rank(name: String, rank: String, score: String, date: String, level: String) {
    def json = {
      import JSONLiteral._
      O("name" -> name,
        "rank" -> rank,
        "score" -> score,
        "date" -> date,
        "level" -> level)
    }
  }

  def fetch(url: String): String = {
    try {
      Source.fromInputStream(new URL(url).openStream, "Shift_JIS").mkString
    } catch {
      case _ => ""
    }
  }

  def parse(s: String) = {
    def parse1(tr: Node) =
      (tr \\ "td") map(n => n.attribute("class").get.text -> n.text) toMap
    val ns = new TagSoupFactoryAdapter loadString(s)
    (ns \\ "table" \\ "tr").drop(1).map(parse1).map(m => Rank(m("name"), m("rank"), m("score"), m("date"), m("level")))
  }

  def json(ranks: Seq[{def json: JSONLiteral.JSONValue }]) = {
    import JSONLiteral._
    JSONLiteral.toString(JSONArray(ranks.map(_.json).toList))
  }

  def buildURL(s: String) = "http://miku.sega.jp/arcade/ranking_" + s + ".php"

  lazy val indexURL = "http://miku.sega.jp/arcade/ranking_index.html"

  def parseIndex(s: String) = {
    val pat = """ranking_(\d+)\.php.*""".r
    val ns = new TagSoupFactoryAdapter loadString(s)
    (ns \\ "a").map(_.attribute("href")).filter(_.isDefined).map(_.get.text).collect{case pat(no) => no}
  }

  def main(args: Array[String]) {
    val r = parse(fetch(buildURL("015_hard")))
    println(r.head)
    println(json(r))
  }

}
