package ya.divaac

import org.specs._
import com.google.appengine.tools.development.testing._
import com.google.appengine.api.datastore._
import com.google.appengine.api.memcache._
import sage._

import DivaacRank2._
import KeyFactory._
import DateUtils._

class Store2Specs extends Specification {
  val helper = new LocalServiceTestHelper(
    new LocalMemcacheServiceTestConfig(),
    new LocalDatastoreServiceTestConfig()
  )

  implicit val datastoreService = DatastoreServiceFactory.getDatastoreService
  val memcacheService = MemcacheServiceFactory.getMemcacheService

  "player" should {
    doBefore{ helper.setUp }
    doAfter{ helper.tearDown }
    "save" >> {
      val p = Player("name", "lv")
      Player.save(Seq(p))
      val e = datastoreService.get(createKey("Player", "name__lv"))
      e must notBeNull;
      e.getKey.getName must beEqual("name__lv")
      e.property[String]("name") must beEqual(Some("name"))
      e.property[String]("level") must beEqual(Some("lv"))
      e.isUnindexedProperty("level") must beTrue;
      Player.ps.read(e) must beEqual(Some(p))
    }

    "save already stored" in {
      val p = Player("name", "lv", asCalendar(2010,8,1).getTime)
      val p2 = Player("name", "lv", asCalendar(2010,8,2).getTime)
      Player.save(Seq(p))
      Player.save(Seq(p2))

      Player.lookup(Player.key("name", "lv")) must beEqual(Some(p))
    }
  }

  "song" should {
    doBefore{ helper.setUp }
    doAfter{ helper.tearDown }
    "all" >> {
      val ss = for(n <- Seq("a","b","c"); k <- Difficulties) yield Song(n+"_"+k, n)
      Song.save(ss:_*)
      Song.all().values must haveTheSameElementsAs(ss)
    }
  }
}
