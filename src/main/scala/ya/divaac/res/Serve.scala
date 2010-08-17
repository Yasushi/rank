package ya.divaac.res

import ya.divaac._
import DivaacRank2._
import AppengineUtils.Memcache._

class Serve extends BaseServlet {
  lazy val fetch = Memoize1('Serve_fetch, (fetchImpl _))
  def fetchImpl(key: String) =
    fetchRanking(key).map(_.toRanking).map(_.json).map(JSONLiteral.toString)

  get("/:key") {
    JSON(fetch(params("key")))
  }

}
