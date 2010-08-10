package ya.divaac

import javax.servlet.http._

import DivaacRank2._
import Utils._

class Select extends HttpServlet {

  override def doGet(req: HttpServletRequest, resp: HttpServletResponse) {
    try {
      Option(req.getPathInfo).map(_.stripPrefix("/").split("/").toList) match {
        case Some("song" :: songKey :: opts) =>
          log(format("songKey: %s opts: %s", songKey, opts))
        opts.headOption match {
          case Some(rankingDate) =>
            printJSON(Ranking.lookupAndToJSON(songKey, rankingDate), req, resp)
          case None =>
            printJSON(Ranking.lookupLatestAndToJSON(songKey), req, resp)
        }
        case Some("player" :: name :: opts) =>
          log(format("player: %s opts: %s", name, opts))
          printJSON(Player.findRecordsByNameToJson(name, opts.headOption.getOrElse(DateUtils.rankingDate())), req, resp)
        case Some("songlist" :: Nil) =>
          log("songlist")
          printJSON(Song.allToJson(), req, resp)
        case _ =>
          resp.setStatus(HttpServletResponse.SC_NO_CONTENT)
      }
      log(format("done. %s", req.getPathInfo))
    } catch {
      case ex =>
        log("error", ex)
        resp.setContentType("text/plain")
        resp.setCharacterEncoding("UTF-8")
        resp.getWriter.print("error" + ex.getMessage)
    }
  }

}
