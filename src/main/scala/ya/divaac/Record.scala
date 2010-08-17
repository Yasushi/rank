package ya.divaac

import scala.collection.JavaConversions._
import java.util.Date
import sage._
import com.google.appengine.api.datastore.Key
import com.google.appengine.api.datastore.DatastoreService

import DivaacRank2._
import AppengineUtils.Datastore
import AppengineUtils.Memcache._

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
