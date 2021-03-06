package ya.divaac

import java.util.Date
import sage._

import DivaacRank2._
import AppengineUtils.Datastore
import AppengineUtils.Memcache._

case class Ranking(song: Song, records: Seq[Record] = Seq.empty,
                   ts: Date = new Date) {
  lazy val rankingDate = DateUtils.rankingDate(ts)
  lazy val key = Ranking.key(song, rankingDate)

  lazy val json = {
    import JSONLiteral._
    O("songKey" -> song.key,
      "songName" -> song.name,
      "rankingDate" -> rankingDate,
      "records" ->
      A(records.zipWithIndex.map{case(r, i) => r.json(i+1)}:_*)
    )
  }

  def pagedJsonString(offset: Int, limit: Int) = {
    import JSONLiteral._
    JSONLiteral.toString(
      O("songKey" -> song.key,
        "songName" -> song.name,
        "rankingDate" -> rankingDate,
        "total" -> records.size,
        "records" ->
        A(records.zipWithIndex.drop(offset).take(limit).map{case(r, i) => r.json(i+1)}:_*)))
  }
}
object Ranking extends Log {
  object ps extends DBase[Ranking]("Ranking") {
    def * = "song".prop[String] :: "ts".prop[Date] >< ((a _) <-> u)
    def a(songKey: String, ts: Date) =
      Ranking(Song.lookup(songKey).getOrElse(Song(songKey, "")), Seq.empty, ts)
    def u(r: Ranking) = Some(r.song.key, r.ts)
    def key(r: Ranking) = key(r.key)
    def save(rs: Ranking*): Iterable[Keyed[Ranking]] = save(rs)
    def latest(songKey: String) = {
      find.query("__key__" ?> key(songKey+"__2")).query("__key__" ?< key(songKey+"__3")).query("__key__" desc).fetch(_.limit(1)).iterable.headOption
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
      val Some(Keyed(pk, _)) =
        ps.lookup(ps.key(r)) orElse ps.save(r).headOption
      Record.save(r.records, pk)
      tx.commit
    }
  }
  def delete(song: Song, rankingDate: String) {
    import scala.collection.JavaConversions._
    Datastore.withTx { tx =>
      val rankingKey = ps.key(key(song, rankingDate))
      val recordKeys = Record.ps.find.query(_.setAncestor(rankingKey)).fetch(_.prefetchSize(300).chunkSize(300)).keys
      datastoreService.delete(asIterable(recordKeys))
      datastoreService.delete(rankingKey)
      tx.commit
      info("deleted ranking {}, records {}.", rankingKey, recordKeys.size)
    }
  }
  def key(song: Song, rankingDate: String) =
    format("%s__%s", song.key, rankingDate)

  lazy val RANKING_KEY_PAT = """(.*)__(\d+)""".r
  def decodeKey(key: String) = key match {
    case RANKING_KEY_PAT(song, date) =>
      Some(Song.lookup(song).getOrElse(Song(song, "")), date)
    case _ => None
  }

  lazy val lookup = Memoize1('Ranking_lookup, (lookupImpl _).tupled)
  def lookupImpl(songKey: String, rankingDate: Option[String]) = {
    rankingDate match {
      case None =>
        ps.latest(songKey).map(r => r.value.copy(records = Record.lookup(r.key).toSeq))
      case Some(date) =>
        val key = ps.key(format("%s__%s", songKey, date))
        ps.lookup(key).map(_.value.copy(records = Record.lookup(key).toSeq))
    }
  }

  lazy val lookupLatest = Memoize1('Ranking_lookupLatest, lookupLatestImpl)
  def lookupLatestImpl(songKey: String) = lookup(songKey, None)

  lazy val lookupAndToJSON =
    Memoize1('Ranking_lookupJson, (lookupAndToJSONImpl _).tupled)
  def lookupAndToJSONImpl(songKey: String, rankingDate: Option[String]) = {
    lookup(songKey, rankingDate) map(_.json) map(JSONLiteral.toString)
  }
}
