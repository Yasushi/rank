package ya.divaac

import org.specs._
import com.google.appengine.tools.development.testing._
import com.google.appengine.api.datastore._
import com.google.apphosting.api.ApiProxy
import java.util.{Calendar, Date}
import sage._
import DivaacRank._
import Persist._
import scala.collection.JavaConversions._

class StoreSpecs extends Specification {
  val delegate = new LocalServiceTestConfig {
    def setUp {ApiProxy.setDelegate(new AncestorQueryEnabler)}
    def tearDown {}
  }
  val helper = new LocalServiceTestHelper(
    new LocalDatastoreServiceTestConfig(), delegate
  )

  implicit val datastoreService = DatastoreServiceFactory.getDatastoreService

  "persist" should {
    doBefore{ helper.setUp }
    doAfter{ helper.tearDown }
    val entries = (1 to 10) map(i => Rank(format("name%d", i), format("%d位", i), (100 - i).toString, "2010/1/1", "Lv 1")) reverse
    val ranking = Ranking("name", "001", "hard", entries)

    "mapping" in {
      import metascala.HLists._
      val fk = KeyFactory.stringToKey(newFetchKey)
      val rr = ranking.entries.map(r => new RankRecord(ranking, r, fk))
      val e = new Entity("RankRecord")
      e.setProperty("songName", "name")
      e.setProperty("songNo", "001")
      e.setProperty("difficulty", "hard")
      e.setProperty("player", "name10")
      e.setProperty("rank", "10位")
      e.setProperty("score", "90")
      e.setProperty("date", "2010/1/1")
      e.setProperty("level", "Lv 1")
      e.setProperty("order", 10.toLong.asInstanceOf[java.lang.Long])
      e.setProperty("fetchKey", fk)

      RankRecordPS.write(rr.head, new Entity("RankRecord")).getProperties must beEqual(e.getProperties)
      RankRecordPS.read(e) must beLike {
        case Some(r) => r must beEqual(rr.head)
      }
    }

    "save and load" in {
      Persist.save(ranking)

      import dsl._

      println(FetchDate.find.iterable)
      println(FetchLogPS.find.iterable)
      println(RankRecordPS.find.query("order" asc).iterable)
      println("--")
      println(Persist.loadLatest("001", "hard"))

    }
  }
}
