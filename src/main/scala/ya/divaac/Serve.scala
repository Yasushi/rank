package ya.divaac

import javax.servlet.http._
import com.google.appengine.api.memcache._

object Util {
  val memcache = MemcacheServiceFactory.getMemcacheService

  def memoB(key: String, expire: Int = 600)(f: => Boolean) =
    memo(key, expire)(_ => f.toString).toBoolean

  def memo(key: String, expire: Int = 600)(f: String => String) =
    Option(memcache.get(key)) match {
      case Some(value) => value.toString
      case None => {
        val value = f(key)
        if (!key.isEmpty && key != "[]")
          memcache.put(key, value, Expiration.byDeltaSeconds(expire))
        value
      }
    }

  def printJSON(json: String, req: HttpServletRequest, resp: HttpServletResponse) {
    Option(req.getParameter("callback")) match {
      case None => {
        resp.setContentType("text/json")
        resp.setCharacterEncoding("UTF-8")
        resp.getWriter.print(json)
      }
      case Some(callback) => {
        resp.setContentType("text/javascript")
        resp.setCharacterEncoding("UTF-8")
        resp.getWriter.print(format("%s(%s)", callback, json))
      }
    }
    resp.getWriter.flush
    resp.getWriter.close
  }

  val noPat = """\d+""".r
  val diffPat = """hard|extreme""".r

}

class Serve extends HttpServlet {
  import DivaacRank._
  import Util._
  def fetchRanksJson(s: String) = {
    assert(!s.isEmpty)
    log("key: " +s)
    def src = memo(s, 3600)((buildURL _) andThen fetch)
    memo("json/"+s, 600)(_ => parse(src).map(json).getOrElse(""))
  }

  override def doGet(req: HttpServletRequest, resp: HttpServletResponse) {
    req.getPathInfo match {
      case key if key != null && key.length > 1 => {
        fetchRanksJson(key.drop(1)) match {
          case json if json.isEmpty =>
            resp.setStatus(HttpServletResponse.SC_NO_CONTENT)
          case json =>
            printJSON(json, req, resp)
        }
      }
      case _ =>
        resp.setStatus(HttpServletResponse.SC_NO_CONTENT)
    }
  }
}
