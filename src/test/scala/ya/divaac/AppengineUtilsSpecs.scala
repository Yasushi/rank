package ya.divaac

import org.specs._
import com.google.appengine.tools.development.testing._
import com.google.appengine.api.datastore._
import com.google.appengine.api.memcache._
import scala.compat.Platform

class AppengineUtilsSpecs extends Specification {
  val helper = new LocalServiceTestHelper(
    new LocalDatastoreServiceTestConfig(),
    new LocalMemcacheServiceTestConfig()
  )

  implicit val datastoreService = DatastoreServiceFactory.getDatastoreService

  "datastore" should {
    doBefore{ helper.setUp }
    doAfter{ helper.tearDown }

    "getkinds" in {
      datastoreService.put(new Entity("FetchDate"))
      datastoreService.put(new Entity("Rank"))
      datastoreService.put(new Entity("Ranking"))
      val kinds = AppengineUtils.Datastore.getKinds
      println(kinds)
      kinds must haveTheSameElementsAs(List("FetchDate", "Ranking", "Rank"))
    }
  }

  "memcache" should {
    doBefore{ helper.setUp }
    doAfter{ helper.tearDown }
    val m = MemcacheServiceFactory.getMemcacheService
    "putget" in {
      m.put("putget", "value")
      m.contains("putget") must beTrue;
      m.get("putget") must beEqual("value")
    }
    "case class" in {

      val e = MemcacheValue("xValue", "yValue")
      m.put("key", e)

      m.get("key").asInstanceOf[MemcacheValue] must beEqual(e)
    }

    "memoize" in {
      def newMV(x: String) = MemcacheValue(x, Platform.currentTime.toString)
      val memo = AppengineUtils.Memcache.Memoize1(newMV)

      val v1 = memo("1")
      m.contains("1") must beTrue;
      m.get("1") must beLike {
        case MemcacheValue("1", y) if y == v1.y => true
      }
      newMV("1") must notBe(v1)
    }
  }

}

case class MemcacheValue(x: String, y: String)
