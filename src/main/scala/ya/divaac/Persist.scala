package ya.divaac

import DivaacRank._

import java.util.{Date, TimeZone}
import java.text.SimpleDateFormat
import com.google.appengine.api.datastore._
import sage._
import metascala.HLists._
import metascala.Nats._
import scala.collection.JavaConversions._

object Persist {
  implicit val datastoreService = DatastoreServiceFactory.getDatastoreService

  def withTx[T](f: (Transaction => T)) = {
    val tx = datastoreService.beginTransaction
    try {
      f(tx)
    } finally {
      Option(datastoreService.getCurrentTransaction(tx)).filter(_.isActive).foreach(_.rollback)
    }
  }

  case class RankRecord(songName: String, songNo: String, difficulty: String,
                        player: String, rank: String, score: String,
                        date: String, level: String,
                        order: Long, createDate: Date, rankingId: String) {
    def this(rk: Ranking, r: Rank, created: Date = new Date) = this(rk.songName, rk.songNo, rk.difficulty, r.player, r.rank, r.score, r.date, r.level, r.order, created, DateUtils.rankingId(created))

    def toRanking(e: Seq[Rank] = Seq.empty) = Ranking(songName, songNo, difficulty, e)
    def toRank = Rank(player, rank, score, date, level)

    def json = {
      val df = new SimpleDateFormat("yyyy-MM-dd'T'HH:mmZ")
      df.setTimeZone(TimeZone.getTimeZone("JST"))
      import JSONLiteral._
      O("songName" -> songName,
        "songNo" -> songNo,
        "difficulty" -> difficulty,
        "player" -> player,
        "rank" -> rank,
        "score" -> score,
        "date" -> date,
        "level" -> level,
        "order" -> order,
        "createDate" -> df.format(createDate),
        "rankingId" -> rankingId)
    }
  }

  private implicit def pimp11[A, B, C, D, E, F, G, H, I, J, K, Z](f: (A, B, C, D, E, F, G, H, I, J, K) => Z) = new {
    def m: A :: B :: C :: D :: E :: F :: G :: H :: I :: J :: K :: HNil => Z = list => f(list.nth[_0], list.nth[_1], list.nth[_2], list.nth[_3], list.nth[_4], list.nth[_5], list.nth[_6], list.nth[_7], list.nth[_8], list.nth[_9], list.nth[_10])

    def <->(unapply: Z => Option[(A,B,C,D,E,F,G,H,I,J,K)]) =
      (m, (z: Z) => unapply(z).get match { case (a,b,c,d,e,f,g,h,i,j,k) => a :: b :: c :: d :: e :: f :: g :: h :: i :: j :: k :: HNil })
  }

  object RankRecordPS extends Base[RankRecord]("RankRecord") {
    def * = "songName".prop[String] :: "songNo".prop[String] :: "difficulty".prop[String] :: "player".prop[String] :: "rank".prop[String] :: "score".prop[String] :: "date".prop[String] :: "level".prop[String] :: "order".prop[Long] :: "createDate".prop[Date] :: "rankingId".prop[String] >< (RankRecord <-> RankRecord.unapply)
    def findBySong(no: String, d: String, rkId: String) =
      find.query("songNo" ?== no).query("difficulty" ?== d).query("rankingId" ?== rkId).query("order" asc)

    def findByPlayer(player: String) =
      find.query("player" ?== player).query("rankingId" desc)

    def findByPlayerAndSong(player: String, songNo: String,
                            difficulty: String) =
      findByPlayer(player).query("songNo" ?== songNo).query("difficulty" ?== difficulty)

    def findByPlayerAndRankingId(player: String, rkId: String) =
      find.query("player" ?== player).query("rankingId" ?== rkId).query("songNo" desc)

    def isRankingExists(no: String, d: String, rkId: Option[String] = None) =
      (find.query("songNo" ?== no).query("difficulty" ?== d).query("rankingId" ?== rkId.getOrElse(DateUtils.rankingId())).fetch(_.limit(1)).iterable).headOption.isDefined

  }

  def isRankingExists(songNo: String, difficulty: String, rankingId: Option[String] = None) =
    RankRecordPS.isRankingExists(songNo, difficulty, rankingId orElse Some(DateUtils.rankingId()))

  def save(rk: Ranking, force: Boolean = false): Boolean = {
    val created = new Date
    withTx { tx =>
      if(force || !isRankingExists(rk.songNo, rk.difficulty, Some(DateUtils.rankingId(created)))) {
        val parent = datastoreService.allocateIds("Ranking", 1).iterator.next
        val keys = datastoreService.allocateIds(parent, RankRecordPS.kind, rk.entries.size).iterator.toIterable
        val rrs = rk.entries.map(r => new RankRecord(rk, r, created))
        val es = rrs zip keys map{case (t, k) => RankRecordPS.keyedEntity(t, k)}
        datastoreService.put(asIterable(es))
        tx.commit
        true
      } else
        false
    }
  }

  def findBySong(songNo: String, difficulty: String, rankingId: String = DateUtils.rankingId()) =
    toRanking(RankRecordPS.findBySong(songNo, difficulty, rankingId).iterable).headOption

  def findByPlayer(player: String) =
    RankRecordPS.findByPlayer(player).iterable.map(_.value)

  def findByPlayerAndSong(player: String, songNo: String, difficulty: String) =
    RankRecordPS.findByPlayerAndSong(player, songNo, difficulty).fetch(_.limit(30)).iterable.map(_.value)

  def findByPlayerAndRankingId(player: String, rankingId: String = DateUtils.rankingId()) =
    RankRecordPS.findByPlayerAndRankingId(player, rankingId).iterable.map(_.value)

  def toRanking(rrs: Iterable[Keyed[RankRecord]]) = {
    rrs.map(_.value).groupBy(_.toRanking()).map {
      case (rk, rs) => rk.copy(entries=rs.map(_.toRank).toSeq)
    }
  }
}

