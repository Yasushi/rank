package ya.divaac

import java.net.URL
import scala.io.Source
import scala.xml._
import AppengineUtils.Memcache.{Memoize0, Memoize1}

object DivaacRank2 extends Log {
  case class Player(name: String, level: String) {
    lazy val key = Player.key(name, level)
  }
  object Player {
    def key(name: String, level: String) = format("%s__%s", name, level)
  }
  case class Record(song: Song, order: Long, score: Long, player: Player,
                    recordDate: String) {
    lazy val key = format("%s__%03d", song.key, order)
  }
  case class Song(key: String, name: Option[String])
  case class Ranking(song: Song, key: String, records: Record*)

  def using[A <: { def close() }, B](resource: A)(f: A => B): B = {
    try {
      f(resource)
    } finally {
      resource.close()
    }
  }

  lazy val fetch = Memoize1(fetchImpl)
  def fetchImpl(url: String): String = {
    try {
      using(Source.fromInputStream(new URL(url).openStream, "Shift_JIS"))(_.mkString)
    } catch {
      case e: Throwable => {
        warn("fetch failed. url: " + url, e)
        ""
      }
    }
  }

  lazy val indexURL = "http://miku.sega.jp/arcade/ranking_index.html"
  lazy val rankingPageURLPattern = """ranking_(.+?)\.php.*""".r
  lazy val Difficulties = List("hard", "extreme")

  lazy val fetchSongKeys = Memoize0(fetchSongKeysImpl, "fetchSongKeys")
  def fetchSongKeysImpl() = {
    try {
      val ns = new TagSoupFactoryAdapter loadString(fetch(indexURL))
      ns \\ "div" find(x => (x \ "@id" text) == "musicList") headOption match {
        case None => Seq.empty
        case Some(div) => {
          (div \ "div" \ "ul" \ "li" \ "a" \\ "@href" map(n => rankingPageURLPattern.findFirstMatchIn(n.text)) filter(_.isDefined) map(_.get.group(1)) sorted).distinct.flatMap(s => Difficulties.map(s + "_" + _))
        }
      }
    } catch {
      case ex =>
        error("ranking_index error", ex)
        Seq.empty
    }
  }

  lazy val buildURL = Memoize1({s: String => "http://miku.sega.jp/arcade/ranking_" + s + ".php"})

  case class RawRanking(key: String, songName: String, recoreds: Seq[Map[Symbol, String]])

  lazy val fetchRanking = Memoize1(fetchRankingImpl)
  def fetchRankingImpl(key: String): Option[RawRanking] = {
    try {
      def attrHasValue(ns: NodeSeq, a: String, v: String) =
        ns(_.attribute(a).exists(_.text==v))
      def record(tr: NodeSeq) =
        tr \ "td" map(e => Symbol(e \ "@class" text) -> e.text) toMap

      val ns = new TagSoupFactoryAdapter loadString(fetch(buildURL(key)))
      val content = attrHasValue(ns \\ "div", "id", "content")
      val songName = content \ "h3" \ "img" \ "@alt" text
      val records = content \ "div"  \ "table" \ "tr" drop(1) map(record)
      Some(RawRanking(key, songName, records))
    } catch {
      case ex =>
        error("ranking parse error", ex)
        None
    }
  }

  /*
  lazy val orderPat = """(\d+)位""".r
  lazy val scorePat = """\d+""".r
  def parseRecord(r: Seq[String], song: Song,
                  players: Map[String, Player]) = r match {
      case Seq(_@orderPat(order), score@scorePat(), name, _, date) if players.contains(name) =>
        Some(Record(song, order.toLong, score.toLong, players(name), date))
      case _ => None
    }
  }
  */
}
