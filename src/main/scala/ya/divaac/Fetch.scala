package ya.divaac

import javax.servlet.http._
import com.google.appengine.api.labs.taskqueue._

class Fetch extends HttpServlet {
  import DivaacRank2._

  val headers = Array("X-AppEngine-QueueName",
                      "X-AppEngine-TaskName",
                      "X-AppEngine-TaskRetryCount")

  def queueAllFetching {
    import TaskOptions.Builder._
    val queue = QueueFactory.getQueue("fetch")
    for (songKey <- fetchSongKeys()) {
      val target = format("/f/%s", songKey)
      log("queued " + target)
      queue.add(url(target).method(TaskOptions.Method.GET))
    }
  }

  def fetchRanking(songKey: String) = {
    val paramString = format("songKey: %s", songKey)
    fetchRankingImpl(songKey) match {
      case Some(ranking) =>
        Ranking.save(ranking.toRanking)
        log("saved. " + paramString)
        true
      case None =>
        false
    }
  }

  def error(resp: HttpServletResponse, message: String, code: Int) {
    log(message)
    resp.sendError(code, message)
  }

  override def doGet(req: HttpServletRequest, resp: HttpServletResponse) {
    log("task header: " + headers.map(h => Option(req.getHeader(h))).filter(_.isDefined).map(_.get).mkString(","))
    Option(req.getPathInfo).map(_.stripPrefix("/").split("/")) match {
      case Some(Array("all")) => queueAllFetching
      case Some(Array(songKey)) =>
        if (!fetchRanking(songKey))
          error(resp, "fetch fail. songKey: " + songKey,
                HttpServletResponse.SC_SERVICE_UNAVAILABLE)
      case _ =>
        log("invalid pathinfo. pathinfo: " + Option(req.getPathInfo).mkString)
        resp.setStatus(HttpServletResponse.SC_NO_CONTENT)
    }
  }

}
