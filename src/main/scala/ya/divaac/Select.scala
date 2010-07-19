package ya.divaac

import javax.servlet.http._

class Select extends HttpServlet {
  import DivaacRank._
  import Util._

  override def doGet(req: HttpServletRequest, resp: HttpServletResponse) {
    Option(req.getPathInfo).map(_.stripPrefix("/").split("/")) match {
      case Some(Array(no@noPat(), diff@diffPat())) => {
        val key = no + "_" + diff
        log("key: " + key)
        val j = memo("s/json/" + key, 600)(_ => Persist.loadLatest(no, diff).map(json).getOrElse(""))
        printJSON(j, req, resp)
      }
      case Some(Array("p", player)) => {
        log("player: " + player)
        val j = memo("s/json/p/" + player, 600)(_ => json(Persist.findByPlayer(player)))
        printJSON(j, req, resp)
      }
      case Some(Array("p", player, no@noPat())) => {
        log(format("player: %s, songNo: %s", player, no))
        val j = memo(format("s/json/p/%s/no/%s", player, no), 600)(_ => json(Persist.findByPlayerAndSong(player, no)))
        printJSON(j, req, resp)
      }
      case Some(Array("p", player, key)) => {
        log(format("player: %s, key: %s", player, key))
        val j = memo(format("s/json/p/%s/k/%s", player, key), 600)(_ => json(Persist.findByPlayerAndKey(player, key)))
        printJSON(j, req, resp)
      }
      case _ =>
        resp.setStatus(HttpServletResponse.SC_NO_CONTENT)
    }
  }

}
