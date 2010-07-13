package ya.divaac

import javax.servlet.http._
import com.google.appengine.api.memcache._


class Serve extends HttpServlet {
  import DivaacRank._
  val memcache = MemcacheServiceFactory.getMemcacheService

  def memo(key: String, expire: Int)(f: String => String) =
    Option(memcache.get(key)) match {
      case Some(value) => value.toString
      case None => {
        val value = f(key)
        memcache.put(key, value, Expiration.byDeltaSeconds(expire))
        value
      }
    } 

  def fetchRanksJson(s: String) = {
    assert(!s.isEmpty)
    log("key: " +s)
    def src = memo(s, 3600)((buildURL _) andThen fetch)
    memo("json/"+s, 600)(_ => json(parse(src)))
  }

  override def doGet(req: HttpServletRequest, resp: HttpServletResponse) {
    req.getPathInfo match {
      case key if key != null && !key.isEmpty => {
        resp.setContentType("text/plain")
        resp.setCharacterEncoding("UTF-8")
        val writer = resp.getWriter
        writer.print(fetchRanksJson(key.drop(1)))
        writer.flush
        writer.close
      }
      case _ =>
        resp.sendError(HttpServletResponse.SC_NO_CONTENT)
    }
  }
}
