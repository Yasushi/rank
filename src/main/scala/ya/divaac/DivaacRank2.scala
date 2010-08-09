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
    val keyPat = """(.*?)__(.*)""".r

    lazy val lookup = Memoize1((lookupImpl _), 3 * 3600)
    def lookupImpl(key: String) = ps lookup(ps.key(key)) map(_.value)
    def save(players: Seq[Player]) {
      ps.notStored(players) match {
        case Seq() =>
        case notStored => ps.save(notStored)
      }
    }
  }
  case class Record(score: Long, player: Player, recordDate: String)
  object Record {
    object ps extends Base[Record]("Record") {
      def * = "score".propNi[Long] :: "player".prop[Key] :: "recordDate".propNi[String] >< ((a _) <-> u)
      def a(score: Long, player: Key, recordDate: String) =
        Record(score, Player.lookup(player.getName).get, recordDate)
      def u(r: Record) =
        Some(r.score, Player.ps.key(r.player), r.recordDate)
      def e(r: Record, order: Long, pk: Key) =
        keyedEntity(r, Datastore.keyById(kind, order, pk))
    }

    def save(rs: Seq[Record], pk: Key) = {
      val es =
        rs.sortBy(_.score * -1).zipWithIndex.map{case(r, i) => ps.e(r, i+1, pk)}
      datastoreService.put(asIterable(es))
    }
    lazy val lookup = Memoize1(lookupImpl)
    def lookupImpl(rankingKey: Key) = ps.childrenOf(rankingKey).map(_.value)
  }
  case class Song(key: String, name: String, ts: Date = new Date)
  object Song {
    object ps extends DBase[Song]("Song") {
      def * = "key".propNi[String] :: "name".propNi[String] :: "ts".prop[Date] >< ((Song.apply _) <-> Song.unapply)
      def key(s: Song) = key(s.key)
    }
    def all = Memoize0(allImpl, "SongAll", 12 * 3600)()
    def allImpl = ps.toMap(ps.find.iterable)
    def lookup(key: String) = all(key)
    def save(songs: Song*) = ps.save(songs)
  }
  case class Ranking(song: Song, records: Seq[Record] = Seq.empty,
                     ts: Date = new Date) {
    lazy val rankingDate = DateUtils.rankingDate(ts)
    lazy val key = format("%s__%s", song.key, rankingDate)
  }
  object Ranking {
    object ps extends DBase[Ranking]("Ranking") {
      import dsl._
      def * = "song".prop[String] :: "ts".prop[Date] >< ((a _) <-> u)
      def a(songKey: String, ts: Date) =
        Ranking(Song.lookup(songKey), Seq.empty, ts)
      def u(r: Ranking) = Some(r.song.key, r.ts)
      def key(r: Ranking) = key(r.key)
      def save(rs: Ranking*): Iterable[Keyed[Ranking]] = save(rs)
    }
    def save(r: Ranking) {
      Song.save(r.song)
      Player.save(r.records.map(_.player))
      Datastore.withTx { tx =>
        val Keyed(pk, _) = ps.save(r).head
        Record.save(r.records, pk)
        tx.commit
      }
    }
    lazy val lookup = Memoize1((lookupImpl _).tupled)
    def lookupImpl(songKey: String, rankingDate: String) = {
      val key = ps.key(format("%s__%s", songKey, rankingDate))
      ps.lookup(key).map(_.value.copy(records = Record.lookup(key).toSeq))
    }
  }

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
