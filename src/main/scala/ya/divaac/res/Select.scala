package ya.divaac.res

import ya.divaac._
import DivaacRank2._

class Select extends BaseServlet {
  get("/songlist") {
    info("songlist")
    JSON(Song.allToJson())
  }

  get("/song/:songKey/?:rankingDate?") {
    val songKey = params("songKey")
    info("songKey: {}, rankingDate: {}, params: {}", songKey, params.get("rankingDate"), params.filterKeys(k => k != "songKey" && k != "rankingDate"))
    val r = Ranking.lookup(songKey, params.get("rankingDate"))
    JSON(r.map(_.pagedJsonString(offset.getOrElse(1) - 1, limit.getOrElse(300))))
  }

  get("/player/:name/?:rankingDate?") {
    val name = params("name")
    val rankingDate = params.get("rankingDate") getOrElse DateUtils.rankingDate()
    info("player: {}, rankingDate: {}", name, rankingDate)
    JSON(Player.findRecordsByNameToJson(name, rankingDate))
  }

}
