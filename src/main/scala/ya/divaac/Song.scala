package ya.divaac

import java.util.Date
import sage._

import DivaacRank2._
import AppengineUtils.Memcache._

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
  lazy val all = Memoize0(allImpl, "SongAll", 6 * 3600)
  def allImpl = ps.toMap(ps.find.iterable)
  lazy val allToJson = Memoize0(allToJsonImpl, "SongAllJson", 6 * 3600)
  def allToJsonImpl =
    Some(JSONLiteral.toString(JSONLiteral.A(all().values.map(_.json).toSeq:_*)))
  lazy val lookup = Memoize1('Song_lookup, lookupImpl)
  def lookupImpl(key: String) = ps.lookup(ps.key(key)).map(_.value)
  def save(songs: Song*) = ps.save(songs)
}
