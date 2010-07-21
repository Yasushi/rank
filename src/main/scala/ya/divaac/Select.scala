package ya.divaac

import javax.servlet.http._

class Select extends HttpServlet {
  import DivaacRank._
  import Util._

  override def doGet(req: HttpServletRequest, resp: HttpServletResponse) {
    Option(req.getPathInfo).map(_.stripPrefix("/").split("/")) match {
      case Some(Array(no@noPat(), diff@diffPat())) => {
        val key = no + "/" + diff
        log("key: " + key)
        val j = memo("s/json/" + key, 1800)(_ => Persist.findBySong(no, diff).map(json).getOrElse(""))
        printJSON(j, req, resp)
      }
      case Some(Array(no@noPat(), diff@diffPat(), rankingId)) => {
        val key = format("%s/%s/%s", no, diff, rankingId)
        log("key: " + key)
        val j = memo("s/json/" + key, 1800)(_ => Persist.findBySong(no, diff, rankingId).map(json).getOrElse(""))
        printJSON(j, req, resp)
      }
      case Some(Array("p", player)) => {
        log("player: " + player)
        val j = memo("s/json/p/" + player, 600)(_ => json(Persist.findByPlayer(player)))
        printJSON(j, req, resp)
      }
      case Some(Array("p", player, no@noPat(), diff@diffPat())) => {
        log(format("player: %s, songNo: %s, difficulty: %s", player, no, diff))
        val j = memo(format("s/json/p/%s/%s/%s", player, no, diff), 1800)(_ => json(Persist.findByPlayerAndSong(player, no, diff)))
        printJSON(j, req, resp)
      }
      case Some(Array("p", player, rankingId)) => {
        log(format("player: %s, rankingId: %s", player, rankingId))
        val j = memo(format("s/json/p/%s/%s", player, rankingId), 1800)(_ => json(Persist.findByPlayerAndRankingId(player, rankingId)))
        printJSON(j, req, resp)
      }
      case _ =>
        resp.setStatus(HttpServletResponse.SC_NO_CONTENT)
    }
  }

}
