package ya.divaac

import javax.servlet.http._
import com.google.appengine.api.datastore._
import sage.dsl._
import scala.collection.JavaConversions._
import Util._
import FetchOptions.Builder._
import com.google.appengine.api.labs.taskqueue._

class Clear extends HttpServlet {
  val ds = DatastoreServiceFactory.getDatastoreService

  def del(q: Query) = {
    val keys: java.lang.Iterable[Key] =
      asIterable(ds.prepare(q.setKeysOnly).asIterable(withLimit(500).prefetchSize(500).chunkSize(500)).map(_.getKey))
    ds.delete(keys)
    val mes = format("%s %d recoreds deleted.", q.getKind, keys.size)
    log(mes)
    mes
  }

  def all = {
    val keys: java.lang.Iterable[Key] = asIterable(ds.prepare(new Query("RankRecord").setKeysOnly).asIterable(withLimit(500).prefetchSize(500).chunkSize(500)).map(_.getKey))

    ds.delete(keys)
    val mes = format("RankRecord %d recoreds deleted.", keys.size)
    log(mes)
    if (keys.size > 0) {
      import TaskOptions.Builder._
      val queue = QueueFactory.getQueue("fetch")
      queue.add(url("/clear/all").method(TaskOptions.Method.GET).countdownMillis(30 * 1000))
      log("queued")
    } else {
      log("done")
    }
  }

  override def doGet(req: HttpServletRequest, resp: HttpServletResponse) {
    resp.setContentType("text/plain")
    val out = resp.getWriter
    val proc = ((del _) andThen out.print)
    Option(req.getPathInfo).map(_.stripPrefix("/").split("/")) match {
      case Some(Array("mc")) => memcache.clearAll
      case Some(Array("all")) => all
      case Some(Array(no@noPat(), diff@diffPat())) =>
        proc((("songNo" ?== no) andThen ("difficulty" ?== diff))(new Query("RankRecord")))
      case Some(Array(no@noPat())) =>
        proc(("songNo" ?== no)(new Query("RankRecord")))
      case Some(Array(diff@diffPat())) =>
        proc(("difficulty" ?== diff)(new Query("RankRecord")))
      case _ =>
        out.println("invalid request. " + req.getPathInfo)
    }
    out.flush
    out.close
  }
}
