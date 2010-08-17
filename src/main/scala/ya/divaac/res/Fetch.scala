package ya.divaac.res

import ya.divaac._
import DivaacRank2._
import AppengineUtils.Memcache._
import com.google.appengine.api.labs.taskqueue._

class Fetch extends BaseServlet {
  val headers = Array("X-AppEngine-QueueName",
                      "X-AppEngine-TaskName",
                      "X-AppEngine-TaskRetryCount")

  before {
    info("task header: {}", headers.map(h => Option(request.getHeader(h))).filter(_.isDefined).map(_.get).mkString(","))
  }

  get("/:songKey") {
    val songKey = params("songKey")
    val paramString = "songKey: %s".format(songKey)
    info("fetching {}", paramString)
    fetchRankingImpl(songKey) match {
      case Some(ranking) =>
        Ranking.save(ranking.toRanking)
        info("saved. {}", paramString)
      case None =>
        halt(503, "fetch fail. " + paramString)
    }
  }

  lazy val queue = QueueFactory.getQueue("fetch")
  get("/all") {
    import TaskOptions.Builder._
    for (songKey <- fetchSongKeys()) {
      val target = format("/f/%s", songKey)
      info("queued {}", target)
      queue.add(url(target).method(TaskOptions.Method.GET))
    }
  }

  get("/marg_hard") {
    info("workaround marg_hard. re-queued /f/maeg_hard")
    queue.add(TaskOptions.Builder.url("/f/maeg_hard").method(TaskOptions.Method.GET))
  }

}
