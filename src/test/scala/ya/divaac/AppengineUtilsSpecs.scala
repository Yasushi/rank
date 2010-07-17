package ya.divaac

import org.specs._
import com.google.appengine.tools.development.testing._
import com.google.appengine.api.datastore._

class AppengineUtilsSpecs extends Specification {
  val helper = new LocalServiceTestHelper(
    new LocalDatastoreServiceTestConfig()
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

}
