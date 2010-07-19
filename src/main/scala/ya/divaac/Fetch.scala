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
    val index = memo("index", 3600)(_ => fetch(indexURL))
    val fk = Persist.newFetchKey
    val queue = QueueFactory.getQueue("fetch")
    for (no <- parseIndex(index); diff <- List("hard", "extreme")) {
      val target = format("/f/%s/%s/%s", no, diff, fk)
      log("queued " + target)
      queue.add(url(target).method(TaskOptions.Method.GET))
    }
  }

  def fetchRanking(songNo: String, difficulty: String, key: Option[String]) {
    val isValidKey = key.map(k => memoB("f/k/" + k)(Persist.validateFetchKey(k))) getOrElse true
    if (!isValidKey) {
      log(format("skip old fetchkey. songNo: %s, difficulty: %s, key: %s",
                 songNo, difficulty, key))
      return
    }

    val isStaled = memoB(format("f/s/%s/%s", songNo, difficulty))(Persist.isStaled(songNo, difficulty))
    if (!isStaled) {
      log(format("skip already fetched. songNo: %s, difficulty: %s, key: %s",
                 songNo, difficulty, key))
      return
    }

    val fk = key getOrElse(Persist.newFetchKey)
    parse(src(songNo + "_" + difficulty)) match {
      case Some(ranking) => {
        Persist.save(ranking, fk)
        log(format("saved. songNo: %s, difficulty: %s, key: %s, fk: %s",
                   songNo, difficulty, key, fk))
      }
      case None =>
        log(format("fetch fail. songNo: %s, difficulty: %s, key: %s, fk: %s",
                   songNo, difficulty, key, fk))
    }
  }

  override def doGet(req: HttpServletRequest, resp: HttpServletResponse) {
    log("task header: " + headers.map(h => Option(req.getHeader(h))).filter(_.isDefined).map(_.get).mkString(","))
    Option(req.getPathInfo).map(_.stripPrefix("/").split("/")) match {
      case Some(Array("all")) => queueAllFetching
      case Some(Array(no@noPat(), diff@diffPat(), key)) =>
        fetchRanking(no, diff, Option(key))
      case Some(Array(no@noPat(), diff@diffPat())) =>
        fetchRanking(no, diff, None)
      case _ =>
        log("invalid pathinfo. pathinfo: " + Option(req.getPathInfo).mkString)
        resp.setStatus(HttpServletResponse.SC_NO_CONTENT)
    }
  }

}
