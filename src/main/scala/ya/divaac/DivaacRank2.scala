package ya.divaac

import java.net.URL
import java.util.Date
import scala.io.Source
import scala.xml._
import scala.collection.JavaConversions._
import sage._
import AppengineUtils.Memcache.{Memoize0, Memoize1}
import AppengineUtils.Datastore
import com.google.appengine.api.datastore.Key
import com.google.appengine.api.datastore.DatastoreService

object DivaacRank2 extends Log {
  implicit val datastoreService = AppengineUtils.Datastore.datastoreService

  lazy val fetch = Memoize1('fetch, fetchImpl)
  def fetchImpl(url: String): String = {
    try {
      JSONLiteral.using(Source.fromInputStream(new URL(url).openStream, "Shift_JIS"))(_.mkString)
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

  def buildURL(key: String) = format("http://miku.sega.jp/arcade/ranking_%s.php", key)

  case class RawRanking(songKey: String, songName: String, records: Seq[Map[Symbol, String]]) {
    def map2Record(m: Map[Symbol, String]) = try {
      val score = m('score).toLong
      val player = Player(m('name), m('level))
      Some(Record(score, player, m('date)))
    } catch {
      case e =>
        warn(format("record parse error. (%s)", m), e)
        None
    }

    lazy val song = Song(songKey, songName)
    private lazy val rankingTemplate = Ranking(song)
    lazy val toRanking =
      rankingTemplate.copy(records = this.records.flatMap(map2Record))
  }

  lazy val fetchRanking = Memoize1('fetchRanking, fetchRankingImpl)
  def fetchRankingImpl(key: String): Option[RawRanking] = {
    try {
      def attrHasValue(ns: NodeSeq, a: String, v: String) =
        ns(_.attribute(a).exists(_.text==v))
      def record(tr: NodeSeq) =
        tr \ "td" map(e => Symbol(e \ "@class" text) -> e.text) toMap
      val src = fetch(buildURL(key))
      if (!src.isEmpty) {
        val ns = new TagSoupFactoryAdapter loadString(src)
        val content = attrHasValue(ns \\ "div", "id", "content")
        val songName = content \ "h3" \ "img" \ "@alt" text
        val records = content \ "div"  \ "table" \ "tr" drop(1) map(record)
        if (!records.isEmpty)
          Some(RawRanking(key, songName, records))
        else
          None
      } else
        None
    } catch {
      case ex =>
        error("ranking parse error", ex)
        None
    }
  }
}
