package ya.divaac

import org.specs._

import scala.io.Source

import com.google.appengine.tools.development.testing._
import com.google.appengine.api.memcache._

import DivaacRank2._

class FetchSpecs extends Specification {
  val helper = new LocalServiceTestHelper(
    new LocalMemcacheServiceTestConfig
  )
  lazy val memcacheService = MemcacheServiceFactory.getMemcacheService

  val songKey = "stst_hard"
  val songSource =
    Source.fromFile("src/test/resources/ranking_stst_hard.php", "Shift_JIS").mkString

  "fetch" should {
    doBefore {
      helper.setUp
      memcacheService.put(buildURL(songKey), songSource)
    }
    doAfter {helper.tearDown}
    "fetchRankingImpl" >> {
      val rr = fetchRanking(songKey)
      rr must beLike {
        case Some(RawRanking("stst_hard", "Star Story", records)) =>
          records must haveSize(300)
      }
    }

    "not found key" >> {
      memcacheService.put(buildURL(songKey), "")
      fetch(buildURL(songKey)) must beEqual("")
      fetchRanking(songKey) must beNone
    }

    "convert" >> {
      val Some(rr) = fetchRanking(songKey)
      val r = rr.toRanking
      r must beLike {
        case Ranking(song, records, _) =>
          song.key must beEqual(songKey)
          records must haveSize(300)
      }
    }
  }
}
