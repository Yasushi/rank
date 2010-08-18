package ya.divaac.res

import ya.divaac._
import DivaacRank2._
import AppengineUtils.Memcache._

class Serve extends BaseServlet {
  lazy val fetch = Memoize1('Serve_fetch, (fetchImpl _).tupled)
  def fetchImpl(key: String, offset: Option[Int], limit: Option[Int]) =
    fetchRanking(key).map(_.toRanking).map(_.pagedJsonString(offset.getOrElse(1) -1, limit.getOrElse(300)))

  get("/:key") {
    JSON(fetch(params("key"), offset, limit))
  }

}
