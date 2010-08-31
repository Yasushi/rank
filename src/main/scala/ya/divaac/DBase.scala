package ya.divaac

import sage._
import com.google.appengine.api.datastore._
import AppengineUtils.Datastore
import scala.collection.JavaConversions._

abstract class DBase[T](val kind: String) extends EntityBase[T] {
  def key(s: String) = Datastore.key(kind, s)
  def keyed(t: T) = Keyed(key(t), t)

  def save(ts: Seq[T])(implicit ds: DatastoreService): Iterable[Keyed[T]] =
    keyedSave(ts map keyed)

  def toMap(keyed: Iterable[Keyed[T]]) =
    keyed.map(k => k.key.getName -> k.value).toMap

  def storedKeys(ts: Seq[T])(implicit ds: DatastoreService) =
    find.query("__key__" ?⊂ ts.map(key)).keys.toSet
  def notStored(ts: Seq[T])(implicit ds: DatastoreService) = {
    val keys = storedKeys(ts)
    ts.filter(t => !keys.contains(key(t)))
  }

  def key(t: T): Key
}
