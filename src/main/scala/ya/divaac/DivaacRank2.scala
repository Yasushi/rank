package ya.divaac

import java.net.URL
import java.util.Date
import scala.io.Source
import scala.xml._
import sage._
import AppengineUtils.Memcache.{Memoize0, Memoize1}
import AppengineUtils.Datastore

object DivaacRank2 extends Log {
  implicit val datastoreService = AppengineUtils.Datastore.datastoreService
  case class Player(name: String, level: String, ts:Date = new Date) {
    lazy val key: String = Player.key(name, level)
  }
  object Player {
    object ps extends DBase[Player]("Player") {
      def * = "name".prop[String] :: "level".propNi[String] :: "ts".prop[Date] >< ((Player.apply _) <-> Player.unapply)
      def key(p: Player) = key(p.key)
    }
    def key(name: String, level: String) = format("%s__%s", name, level)

    lazy val lookup = Memoize1(lookupImpl)
    def lookupImpl(key: String) = ps lookup(ps.key(key)) map(_.value)
    def save(players: Seq[Player]) {
      ps.notStored(players) match {
        case Seq() =>
        case notStored => ps.save(notStored)
      }
    }
  }
  case class Record(song: Song, order: Long, score: Long, player: Player,
                    recordDate: String) {
    lazy val key = format("%s__%03d", song.key, order)
  }
  case class Song(key: String, name: String)
  object Song {
    def getOrNew(key: String, name: String) = Song(key, name)
  }
  case class Ranking(song: Song, key: String, records: Seq[Record])

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

  def buildURL(key: String) = format("http://miku.sega.jp/arcade/ranking_%s.php", key)

  case class RawRanking(key: String, songName: String, records: Seq[Map[Symbol, String]]) {
    def map2Record(m: Map[Symbol, String]) = try {
      val rankPat = """(\d+)位""".r
      val song = Song.getOrNew(key, songName)
      val rankPat(orderStr) = m('rank)
      val score = m('score).toLong
      val player = Player(m('name), m('level))
      Some(Record(song, orderStr.toLong, score, player, m('date)))
    } catch {
      case e =>
        warn(format("record parse error. (%s)", m), e)
        None
    }

    def toRanking: Ranking = {
      val song = Song.getOrNew(key, songName)
      Ranking(Song.getOrNew(key, songName), key, records flatMap(map2Record))
    }
  }

  lazy val fetchRanking = Memoize1(fetchRankingImpl)
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
