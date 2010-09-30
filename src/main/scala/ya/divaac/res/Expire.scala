package ya.divaac.res

import ya.divaac._
import DivaacRank2._
import AppengineUtils.Memcache._
import com.google.appengine.api.labs.taskqueue._

class Expire extends BaseServlet {
  val headers = Array("X-AppEngine-QueueName",
                      "X-AppEngine-TaskName",
                      "X-AppEngine-TaskRetryCount")

  before {
    info("task header: {}", headers.map(h => Option(request.getHeader(h))).filter(_.isDefined).map(_.get).mkString(","))
  }

  get("/:rankingKey") {
    params.get("rankingKey").flatMap(Ranking.decodeKey) match {
      case Some((song, date)) =>
        Ranking.delete(song, date)
        "deleted."
      case None =>
        info("invalid ranking key '{}'.", params.get("rankingKey"))
        "invalid key."
    }
  }

  lazy val queue = QueueFactory.getQueue("expire")
  get("/date/:rankingDate", requestPath.split("/").last.matches("""20\d{6}""")){
    import TaskOptions.Builder._
    for(s <- Song.all().values) {
      val target = format("/expire/%s__%s", s.key, params("rankingDate"))
      info("queued {}", target)
      queue.add(url(target).method(TaskOptions.Method.GET))
    }
  }

}
