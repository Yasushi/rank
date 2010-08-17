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
    info("songKey: {}, rankingDate: {}", songKey, params.get("rankingDate"))
    params.get("rankingDate") match {
      case Some(rankingDate) =>
        JSON(Ranking.lookupAndToJSON(songKey, rankingDate))
      case None =>
        JSON(Ranking.lookupLatestAndToJSON(songKey))
    }
  }

  get("/player/:name/?:rankingDate?") {
    val name = params("name")
    val rankingDate = params.get("rankingDate") getOrElse DateUtils.rankingDate()
    info("player: {}, rankingDate: {}", name, rankingDate)
    JSON(Player.findRecordsByNameToJson(name, rankingDate))
  }

}
