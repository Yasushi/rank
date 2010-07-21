package ya.divaac

import javax.servlet.http._
import com.google.appengine.api.labs.taskqueue._

class Fetch extends HttpServlet {
  import DivaacRank._
  import Util._
  def src(s: String) = memo(s, 3600)((buildURL _) andThen fetch)

  val headers = Array("X-AppEngine-QueueName",
                      "X-AppEngine-TaskName",
                      "X-AppEngine-TaskRetryCount")

  def queueAllFetching {
    import TaskOptions.Builder._
    val index = memo("index", 3600 * 23)(_ => fetch(indexURL))
    val queue = QueueFactory.getQueue("fetch")
    for (no <- parseIndex(index); diff <- List("hard", "extreme")) {
      val target = format("/f/%s/%s", no, diff)
      log("queued " + target)
      queue.add(url(target).method(TaskOptions.Method.GET))
    }
  }

  def fetchRanking(songNo: String, difficulty: String, force: Boolean = false) {
    val paramString = format("songNo: %s, difficulty: %s, force: %s", songNo, difficulty, force)
    parse(src(songNo + "_" + difficulty)) match {
      case Some(ranking) => {
        if(Persist.save(ranking, force))
          log("saved. " + paramString)
        else
          log("skip. already saved. " + paramString)
      }
      case None =>
        log("fetch or parse fail. " + paramString)
    }
  }

  override def doGet(req: HttpServletRequest, resp: HttpServletResponse) {
    log("task header: " + headers.map(h => Option(req.getHeader(h))).filter(_.isDefined).map(_.get).mkString(","))
    Option(req.getPathInfo).map(_.stripPrefix("/").split("/")) match {
      case Some(Array("all")) => queueAllFetching
      case Some(Array(no@noPat(), diff@diffPat())) => fetchRanking(no, diff)
      case _ =>
        log("invalid pathinfo. pathinfo: " + Option(req.getPathInfo).mkString)
        resp.setStatus(HttpServletResponse.SC_NO_CONTENT)
    }
  }

}
