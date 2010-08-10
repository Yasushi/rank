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
  case class Player(name: String, level: String, ts:Date = new Date) {
    lazy val key: String = Player.key(name, level)
  }
  object Player {
    object ps extends DBase[Player]("Player") {
      def * = "name".prop[String] :: "level".propNi[String] :: "ts".prop[Date] >< ((Player.apply _) <-> Player.unapply)
      def key(p: Player) = key(p.key)
      def lookupByName(name: String) = {
        find.query("name" ?== name).query("ts" desc).fetch(_.limit(1)).iterable.headOption
      }
    }
    def key(name: String, level: String) = format("%s|%s", name, level)
    def decodeKey(key: String) = {
      key.split("\\|") match {
        case Array(name, level) => Some(Player(name, level))
        case _ => None
      }
    }

    lazy val lookup = Memoize1('Player_lookup, (lookupImpl _), 3 * 3600)
    def lookupImpl(key: String) = ps lookup(ps.key(key)) map(_.value)
    def save(players: Seq[Player]) {
      ps.notStored(players) match {
        case Seq() =>
        case notStored => ps.save(notStored)
      }
    }

    lazy val findRecordsByNameToJson = Memoize1('Player_findRecordsByNameToJson, (findRecordsByNameToJsonImpl _).tupled)
    def findRecordsByNameToJsonImpl(name: String, rankingDate: String = DateUtils.rankingDate()) = {
      import JSONLiteral._
      Some(JSONLiteral.toString(A(findRecordsByName(name, rankingDate).toSeq.map(_.json):_*)))
    }
    def findRecordsByName(name: String, rankingDate: String = DateUtils.rankingDate()): Iterable[Record] = {
      ps.lookupByName(name) match {
        case None => None
        case Some(Keyed(key, p)) =>
          val rks = Ranking.ps.keysByDate(rankingDate)
          Record.ps.findByPlayer(key, rks)
      }
    }
  }
  case class Record(score: Long, player: Player, recordDate: String, order: Option[Long] = None, song: Option[Song] = None) {
    def json(order: Int) = {
      import JSONLiteral._
      O("name" -> player.name,
        "rank" -> order,
        "score" -> score,
        "date" -> recordDate,
        "level" -> A(player.level.split("\\p{javaWhitespace}", 3).map(string2JSONString):_*))
    }
    def json = {
      import JSONLiteral._
      O("song" -> song.map(_.json).getOrElse(""),
        "name" -> player.name,
        "rank" -> order.mkString,
        "score" -> score,
        "date" -> recordDate,
        "level" -> A(player.level.split("\\p{javaWhitespace}", 3).map(string2JSONString):_*))
    }
  }
  object Record {
    object ps extends Base[Record]("Record") {
      def * = "score".propNi[Long] :: "player".prop[Key] :: "recordDate".propNi[String] >< ((a _) <-> u)
      def a(score: Long, playerKey: Key, recordDate: String) = {
        Record(score, Player.decodeKey(playerKey.getName).get, recordDate)
      }
      def u(r: Record) =
        Some(r.score, Player.ps.key(r.player), r.recordDate)
      def e(r: Record, order: Long, pk: Key) =
        keyedEntity(r, Datastore.keyById(kind, order, pk))
      override def childrenOf(pk: Key)(implicit ds: DatastoreService) = {
        find.query(_.setAncestor(pk)).query("__key__" asc).fetch(_.prefetchSize(300).chunkSize(300)).iterable
      }
      def fromKeyed(k: Keyed[Record]) = {
        val order = k.key.getId
        val Some((song, _)) = Ranking.decodeKey(k.key.getParent.getName)
        k.value.copy(order=Some(order), song=Some(song))
      }
      def findByPlayer(player: Key, rankings: Iterable[Key]): Iterable[Record] = {
        (for (rk <- rankings) yield
          find.query("player" ?== player).query(_.setAncestor(rk)).iterable
        ).flatten.map(fromKeyed)
      }
    }

    def save(rs: Seq[Record], pk: Key) = {
      val es =
        rs.sortBy(_.score * -1).zipWithIndex.map{case(r, i) => ps.e(r, i+1, pk)}
      datastoreService.put(asIterable(es))
    }
    lazy val lookup = Memoize1('Record_lookup,lookupImpl)
    def lookupImpl(rankingKey: Key) = ps.childrenOf(rankingKey).map(_.value)
  }
  case class Song(key: String, name: String, ts: Date = new Date) {
    def json = {
      import JSONLiteral._
      O("key" -> key, "name" -> name)
    }
  }
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

    lazy val json = {
      import JSONLiteral._
      O("songKey" -> song.key,
        "songName" -> song.name,
        "rankingDate" -> rankingDate,
        "records" ->
          A(records.zipWithIndex.map{case(r, i) => r.json(i+1)}:_*)
      )
    }
  }
  object Ranking {
    object ps extends DBase[Ranking]("Ranking") {
      def * = "song".prop[String] :: "ts".prop[Date] >< ((a _) <-> u)
      def a(songKey: String, ts: Date) =
        Ranking(Song.lookup(songKey), Seq.empty, ts)
      def u(r: Ranking) = Some(r.song.key, r.ts)
      def key(r: Ranking) = key(r.key)
      def save(rs: Ranking*): Iterable[Keyed[Ranking]] = save(rs)
      def latest(songKey: String) = {
        find.query("__key__" ?> key(songKey+"__")).query("__key__" desc).fetch(_.limit(1)).iterable.headOption
      }
      lazy val keysByDate = Memoize1('Ranking_ps_keysByDate, keysByDateImpl)
      def keysByDateImpl(rankingDate: String = DateUtils.rankingDate()) = {
        DateUtils.parseRankingDate(rankingDate).map(DateUtils.range) match {
          case Some((start, end)) =>
            find.query("ts" ?> start).query("ts" ?< end).fetch(_.prefetchSize(200).chunkSize(200)).keys
          case _ =>
            Seq.empty
        }
      }
    }
    def save(r: Ranking) {
      Song.save(r.song)
      Player.save(r.records.map(_.player))
      Datastore.withTx { tx =>
        val Some(Keyed(pk, _)) = ps.lookup(ps.key(r)) orElse ps.save(r).headOption
        Record.save(r.records, pk)
        tx.commit
      }
    }
    lazy val RANKING_KEY_PAT = """(.*)__(\d+)""".r
    def decodeKey(key: String) = key match {
      case RANKING_KEY_PAT(song, date) => Some(Song.lookup(song), date)
      case _ => None
    }

    lazy val lookup = Memoize1('Ranking_lookup, (lookupImpl _).tupled)
    def lookupImpl(songKey: String, rankingDate: String) = {
      val key = ps.key(format("%s__%s", songKey, rankingDate))
      ps.lookup(key).map(_.value.copy(records = Record.lookup(key).toSeq))
    }

    lazy val lookupLatest = Memoize1('Ranking_lookupLatest, lookupLatestImpl)
    def lookupLatestImpl(songKey: String) =
      ps.latest(songKey).map(r => r.value.copy(records = Record.lookup(r.key).toSeq))

    lazy val lookupAndToJSON = Memoize1('Ranking_lookupJson, (lookupAndToJSONImpl _).tupled)
    def lookupAndToJSONImpl(songKey: String, rankingDate: String) = {
      lookup(songKey, rankingDate) map(_.json) map(JSONLiteral.toString)
    }
    lazy val lookupLatestAndToJSON = Memoize1('Ranking_lookupLatestJson, lookupLatestAndToJSONImpl)
    def lookupLatestAndToJSONImpl(songKey: String) = {
      lookupLatest(songKey) map(_.json) map(JSONLiteral.toString)
    }
  }

  def using[A <: { def close() }, B](resource: A)(f: A => B): B = {
    try {
      f(resource)
    } finally {
      resource.close()
    }
  }

  lazy val fetch = Memoize1('fetch, fetchImpl)
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
