package ya.divaac

import javax.servlet.http._

import AppengineUtils.Memcache._
import Utils._

class Serve extends HttpServlet {
  lazy val fetch = Memoize1('Serve_fetch, (fetchImpl _))
  def fetchImpl(key: String) =
    DivaacRank2.fetchRanking(key).map(_.toRanking).map(_.json).map(JSONLiteral.toString)

  override def doGet(req: HttpServletRequest, resp: HttpServletResponse) {
    Option(req.getPathInfo).map(_.stripPrefix("/")) match {
      case Some(key) if key != null && key.length > 1 =>
        printJSON(fetch(key), req, resp)
      case _ =>
        resp.setStatus(HttpServletResponse.SC_NO_CONTENT)
    }
  }
}
