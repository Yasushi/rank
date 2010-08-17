package ya.divaac

import java.util.Date
import sage._

import DivaacRank2._
import AppengineUtils.Memcache._

case class Player(name: String, level: String, ts: Date = new Date) {
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

  lazy val lookup = Memoize1('Player_lookup, (lookupImpl _), 6 * 3600)
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
