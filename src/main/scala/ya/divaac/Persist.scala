package ya.divaac

import DivaacRank._

import java.util.Date
import com.google.appengine.api.datastore._
import sage._
import metascala.HLists._
import metascala.Nats._


object Persist {
  implicit val datastoreService = DatastoreServiceFactory.getDatastoreService

  case class RankRecord(songName: String, songNo: String, difficulty: String,
                        player: String, rank: String, score: String,
                        date: String, level: String,
                        order: Long, fetchKey: Key) {
    def this(rk: Ranking, r: Rank, fk: Key) = this(rk.songName, rk.songNo, rk.difficulty, r.player, r.rank, r.score, r.date, r.level, r.order, fk)

    def toRanking(e: Seq[Rank] = Seq.empty) = Ranking(songName, songNo, difficulty, e)
    def toRank = Rank(player, rank, score, date, level)
  }

  case class FetchLog(songNo: String, difficulty: String, fetchKey: Key, date: Date = new Date)

  private implicit def pimp10[A, B, C, D, E, F, G, H, I, J, Z](f: (A, B, C, D, E, F, G, H, I, J) => Z) = new {
    def m: A :: B :: C :: D :: E :: F :: G :: H :: I :: J :: HNil => Z = list => f(list.nth[_0], list.nth[_1], list.nth[_2], list.nth[_3], list.nth[_4], list.nth[_5], list.nth[_6], list.nth[_7], list.nth[_8], list.nth[_9])

    def <->(unapply: Z => Option[(A,B,C,D,E,F,G,H,I,J)]) =
      (m, (z: Z) => unapply(z).get match { case (a,b,c,d,e,f,g,h,i,j) => a :: b :: c :: d :: e :: f :: g :: h :: i :: j :: HNil })
  }

  object RankRecordPS extends Base[RankRecord]("RankRecord") {
    def * = "songName".prop[String] :: "songNo".prop[String] :: "difficulty".prop[String] :: "player".prop[String] :: "rank".prop[String] :: "score".prop[String] :: "date".prop[String] :: "level".prop[String] :: "order".prop[Long] :: "fetchKey".prop[Key] >< (RankRecord <-> RankRecord.unapply)
    def findBySong(no: String, d: String, k: Key) = 
      find.query("songNo" ?== no).query("difficulty" ?== d).query("fetchKey" ?== k).query("order" asc)
  }

  object FetchDate extends Base[Date]("FetchDate") {
    def * = "date".prop[Date]
  }

  object FetchLogPS extends Base[FetchLog]("FetchLog") {
    def * = "songNo".prop[String] :: "difficulty".prop[String] :: "fetchKey".prop[Key] :: "date".prop[Date] >< (FetchLog <-> FetchLog.unapply _)
    def latest(songNo: String, difficulty: String): Option[FetchLog] = {
      import dsl._
      ((find.query("songNo" ?== songNo).query("difficulty" ?== difficulty).query("date" desc).fetch(_.limit(1)) iterable) headOption) map(_.value)
    }
  }

  def newFetchKey = (FetchDate << new Date) match {
    case Keyed(key, date) => KeyFactory.keyToString(key)
  }

  def latestFetchKey = {
    import dsl._
    ((FetchDate.find.query("date" desc).fetch(_.limit(1)) iterable) headOption) map(_.key) map(KeyFactory.keyToString)
  }

  def save(rk: Ranking, fetchKey: String = null) {
    val fk = KeyFactory.stringToKey(Option(fetchKey) getOrElse newFetchKey)
    RankRecordPS <<++ rk.entries.map(r => new RankRecord(rk, r, fk))
    FetchLogPS << FetchLog(rk.songNo, rk.difficulty, fk)
  }

  def loadLatest(songNo: String, difficulty: String) = {
    FetchLogPS.latest(songNo, difficulty) flatMap {
      case FetchLog(_, _, fk, _) => {
        RankRecordPS.findBySong(songNo, difficulty, fk).iterable.map(_.value).groupBy(_.toRanking()).map{
          case (rk, rs) => rk.copy(entries=rs.map(_.toRank).toSeq)
        }.headOption
      }
    }
  }

}
