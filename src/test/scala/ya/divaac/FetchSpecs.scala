package ya.divaac

import org.specs._
import org.specs.Sugar._

import scala.io.Source

import com.google.appengine.tools.development.testing._
import com.google.appengine.api.memcache._

import DivaacRank2._

class FetchSpecs extends Specification {
  val helper = new LocalServiceTestHelper(
    new LocalMemcacheServiceTestConfig
  )
  lazy val memcacheService = MemcacheServiceFactory.getMemcacheService

  val rankingKey = "stst_hard"
  val rankingSource =
    Source.fromFile("src/test/resources/ranking_stst_hard.php", "Shift_JIS").mkString

  "fetch" should {
    doBefore {
      helper.setUp
      memcacheService.put(buildURL(rankingKey), rankingSource)
    }
    doAfter {helper.tearDown}
    "fetchRankingImpl" >> {
      val rr = fetchRanking(rankingKey)
      rr must beLike {
        case Some(RawRanking("stst_hard", "Star Story", records)) =>
          records must haveSize(300)
      }
    }
    "not found key" >> {
      skip("")
    }

    "convert" >> {
      val Some(rr) = fetchRanking(rankingKey)
      val r = rr.toRanking
      r must beLike {
        case Ranking(song, `rankingKey`, records) =>
          song.key must beEqual(rankingKey)
          records must haveSize(300)
      }
    }
  }
}
