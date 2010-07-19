package ya.divaac

import DivaacRank._

import java.util.{Date, TimeZone}
import java.text.SimpleDateFormat
import com.google.appengine.api.datastore._
import sage._
import metascala.HLists._
import metascala.Nats._

object Persist {
  implicit val datastoreService = DatastoreServiceFactory.getDatastoreService

  case class RankRecord(songName: String, songNo: String, difficulty: String,
                        player: String, rank: String, score: String,
                        date: String, level: String,
                        order: Long, fetchKey: Key, fetchDate: Date) {
    def this(rk: Ranking, r: Rank, fk: Keyed[Date]) = this(rk.songName, rk.songNo, rk.difficulty, r.player, r.rank, r.score, r.date, r.level, r.order, fk.key, fk.value)

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
        "fetchDate" -> df.format(fetchDate),
        "fetchKey" -> KeyFactory.keyToString(fetchKey))
    }
  }

  case class FetchLog(songNo: String, difficulty: String, fetchKey: Key, date: Date = new Date)

  private implicit def pimp11[A, B, C, D, E, F, G, H, I, J, K, Z](f: (A, B, C, D, E, F, G, H, I, J, K) => Z) = new {
    def m: A :: B :: C :: D :: E :: F :: G :: H :: I :: J :: K :: HNil => Z = list => f(list.nth[_0], list.nth[_1], list.nth[_2], list.nth[_3], list.nth[_4], list.nth[_5], list.nth[_6], list.nth[_7], list.nth[_8], list.nth[_9], list.nth[_10])

    def <->(unapply: Z => Option[(A,B,C,D,E,F,G,H,I,J,K)]) =
      (m, (z: Z) => unapply(z).get match { case (a,b,c,d,e,f,g,h,i,j,k) => a :: b :: c :: d :: e :: f :: g :: h :: i :: j :: k :: HNil })
  }

  object RankRecordPS extends Base[RankRecord]("RankRecord") {
    def * = "songName".prop[String] :: "songNo".prop[String] :: "difficulty".prop[String] :: "player".prop[String] :: "rank".prop[String] :: "score".prop[String] :: "date".prop[String] :: "level".prop[String] :: "order".prop[Long] :: "fetchKey".prop[Key] :: "fetchDate".prop[Date] >< (RankRecord <-> RankRecord.unapply)
    def findBySong(no: String, d: String, k: Key) =
      find.query("songNo" ?== no).query("difficulty" ?== d).query("fetchKey" ?== k).query("order" asc)

    def findByPlayer(player: String) =
      find.query("player" ?== player).query("fetchDate" desc)

    def findByPlayerAndSong(player: String, songNo: String) =
      findByPlayer(player).query("songNo" ?== songNo)

    def findByPlayerAndKey(player: String, fetchKey: Key) =
      find.query("player" ?== player).query("fetchKey" ?== fetchKey).query("songNo" desc)
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

  def validateFetchKey(key: String) = {
    FetchDate.lookup(KeyFactory.stringToKey(key).getId) collect {
      case Keyed(_, date) =>
        new Date().getTime - date.getTime < 2 * 3600 * 1000
    } getOrElse false
  }

  def isStaled(songNo: String, difficulty: String) = {
    FetchLogPS.latest(songNo, difficulty).collect {
      case FetchLog(_,_,_,date) =>
        new Date().getTime - date.getTime > 24 * 3600 * 1000
    } getOrElse true
  }

  def latestFetchKey = {
    import dsl._
    ((FetchDate.find.query("date" desc).fetch(_.limit(1)) iterable) headOption) map(_.key) map(KeyFactory.keyToString)
  }

  def save(rk: Ranking, fetchKey: String = null) = {
    val fkey = KeyFactory.stringToKey(Option(fetchKey) getOrElse newFetchKey)
    FetchDate.lookup(fkey.getId) match {
      case Some(fk) => {
        RankRecordPS <<++ rk.entries.map(r => new RankRecord(rk, r, fk))
        FetchLogPS << FetchLog(rk.songNo, rk.difficulty, fk.key)
      }
      case None =>
    }
  }

  def loadLatest(songNo: String, difficulty: String) = {
    FetchLogPS.latest(songNo, difficulty) flatMap {
      case FetchLog(_, _, fk, _) => {
        toRanking(RankRecordPS.findBySong(songNo, difficulty, fk).iterable).headOption
      }
    }
  }

  def findByPlayer(player: String) =
    RankRecordPS.findByPlayer(player).iterable.map(_.value)

  def findByPlayerAndSong(player: String, songNo: String) =
    RankRecordPS.findByPlayerAndSong(player, songNo).iterable.map(_.value)

  def findByPlayerAndKey(player: String, fetchKey: String) =
    RankRecordPS.findByPlayerAndKey(player, KeyFactory.stringToKey(fetchKey)).iterable.map(_.value)

  def toRanking(rrs: Iterable[Keyed[RankRecord]]) = {
    rrs.map(_.value).groupBy(_.toRanking()).map {
      case (rk, rs) => rk.copy(entries=rs.map(_.toRank).toSeq)
    }
  }
}

