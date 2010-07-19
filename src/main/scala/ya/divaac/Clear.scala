package ya.divaac

import javax.servlet.http._
import com.google.appengine.api.datastore._
import sage.dsl._
import scala.collection.JavaConversions._
import Util._
import FetchOptions.Builder._

class Clear extends HttpServlet {
  val ds = DatastoreServiceFactory.getDatastoreService

  def del(q: Query) = {
    val keys: java.lang.Iterable[Key] =
      asIterable(ds.prepare(q.setKeysOnly).asIterable(withLimit(301)).map(_.getKey))
    ds.delete(keys)
    format("%d recoreds deleted.", keys.size)
  }

  override def doGet(req: HttpServletRequest, resp: HttpServletResponse) {
    resp.setContentType("text/plain")
    val out = resp.getWriter
    val proc = ((del _) andThen out.print)
    Option(req.getPathInfo).map(_.stripPrefix("/").split("/")) match {
      case Some(Array("fd")) => proc(new Query("FetchDate"))
      case Some(Array("fl")) => proc(new Query("FetchLog"))
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
