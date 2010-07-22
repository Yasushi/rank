package ya.divaac

import javax.servlet.http._
import com.google.appengine.api.datastore._
import com.google.appengine.api.labs.taskqueue._
import sage._
import sage.dsl._
import scala.collection.JavaConversions._
import FetchOptions.Builder._
import java.util.Date

class Convert extends HttpServlet {
  val ds = DatastoreServiceFactory.getDatastoreService

  def exec(cursorString: Option[String]) = {
    def convertToNewRecord(e: Entity) = {
      e.setProperty("fetchKey", null)
      e
    }
    val fo = withLimit(300).startCursor(Cursor.fromWebSafeString(cursorString.getOrElse("")))
    val q = ("fetchKey" ?> "")
    val qrl = ds.prepare(q(new Query("RankRecord"))).asQueryResultList(fo)
    log(format("convert start %s", cursorString))
    if (!qrl.isEmpty) {
      ds.put(asIterable(qrl filter(_.property[Key]("fetchKey").isDefined) map convertToNewRecord))
      log(format("converted %d records. cursor: %s, next: %s.", qrl.size, cursorString, qrl.getCursor.toWebSafeString))
      Some(qrl.getCursor.toWebSafeString)
    } else
      None
  }

  def next(cursorString: Option[String]) {
    import TaskOptions.Builder._
    cursorString match {
      case Some(c) => {
        val queue = QueueFactory.getQueue("fetch")
        queue.add(url("/cv/" + c).method(TaskOptions.Method.GET).countdownMillis(30 * 1000))
        log("queued " + c)
      }
      case None => log("end")
    }
  }

  override def doGet(req: HttpServletRequest, resp: HttpServletResponse) {
    next(exec(Option(req.getPathInfo).flatMap(_.stripPrefix("/").split("/").headOption)))
  }
}
