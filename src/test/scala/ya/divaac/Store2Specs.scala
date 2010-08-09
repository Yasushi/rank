package ya.divaac

import org.specs._

import scala.collection.JavaConversions._
import scala.io.Source

import com.google.appengine.tools.development.testing._
import com.google.appengine.api.datastore._
import com.google.appengine.api.memcache._
import sage._

import DivaacRank2._
import KeyFactory._
import DateUtils._

class Store2Specs extends Specification with util.HmsTimer {
  val helper = new LocalServiceTestHelper(
    new LocalMemcacheServiceTestConfig(),
    new LocalDatastoreServiceTestConfig()
  )

  implicit val datastoreService = DatastoreServiceFactory.getDatastoreService
  val memcacheService = MemcacheServiceFactory.getMemcacheService

  lazy val songKey = "stst_hard"
  lazy val songSource =
    Source.fromFile("src/test/resources/ranking_stst_hard.php", "Shift_JIS").mkString

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

  "save" should {
    doBefore{
      helper.setUp
      memcacheService.put(buildURL(songKey), songSource)
    }
    doAfter{ helper.tearDown }
    "ranking" >> {
      val Some(rr) = fetchRanking(songKey)
      val r = rr.toRanking
      Ranking.save(r)

      val er = datastoreService.prepare(new Query("Ranking")).asSingleEntity
      println(er)
      val err = datastoreService.prepare(new Query("Record", er.getKey)).asIterable
      println(err.head)
      err.size must beEqual(300)
      println(err.drop(102).take(3))

      import Query.FilterOperator._
      val playerKey = createKey("Player", "ふすぃみっちゃん__Lv 106 カンツォーナ")
      val r1 = datastoreService.prepare(new Query("Record", er.getKey).addFilter("player", EQUAL, playerKey)).asSingleEntity

      println(r1)

      val ps = datastoreService.prepare(new Query("Player")).asIterable
      println(ps.head)
      ps.size must beEqual(300)

      val song = datastoreService.prepare(new Query("Song")).asIterable
      println(song.head)
      song.size must beEqual(1)
    }
    "ranking2" >> {
      val Some(rr) = fetchRanking(songKey)
      val r = rr.toRanking
      start
      Ranking.save(r)
      printf("first save: %s\n", stop)
      start
      val rts = asCalendar(r.ts)
      rts += (java.util.Calendar.DATE, 1)
      val r2 = r.copy(ts=rts.getTime)
      printf("copy: %s\n", stop)
      start
      Ranking.save(r2)
      printf("second save: %s\n", stop)

      val rs = datastoreService.prepare(new Query("Ranking")).asIterable
      rs.size must beEqual(2)
      val rr1 = datastoreService.prepare(new Query("Record", rs.head.getKey)).asIterable
      val rr2 = datastoreService.prepare(new Query("Record", rs.last.getKey)).asIterable
      rr1.size must beEqual(300)
      rr2.size must beEqual(300)

      val ps = datastoreService.prepare(new Query("Player")).asIterable
      ps.size must beEqual(300)

      val song = datastoreService.prepare(new Query("Song")).asIterable
      song.size must beEqual(1)
    }
    "ranking read write" >> {
      val Some(rr) = fetchRanking(songKey)
      val ranking = rr.toRanking
      Ranking.save(ranking)

      start
      val ret = Ranking.lookup(songKey, ranking.rankingDate)
      printf("lookup: %s\n", stop)
      ret must beLike {
        case Some(r) =>
          r.song.key must beEqual(songKey)
          r.ts must beEqual(ranking.ts)
          r.records must beEqual(ranking.records)
      }
    }
  }

  "entity spec" should {
    skip("LLAPIの確認用")
    doBefore{helper.setUp}
    doAfter{ helper.tearDown }
    val ds = datastoreService
    "same id" >> {
      val e1 = new Entity(createKey("entity", 1))
      e1.setProperty("name", "e1")
      val e2 = new Entity(createKey("entity", 1))
      e2.setProperty("name", "e2")

      ds.put(e1)
      ds.get(createKey("entity", 1)).property[String]("name") must beEqual(Some("e1"))

      ds.put(e2)
      ds.get(createKey("entity", 1)).property[String]("name") must beEqual(Some("e2"))
    }

    "same id and different parent" >> {
      val pk1 = createKey("parent", 1)
      ds.put(new Entity(pk1))
      val pk2 = createKey("parent", 2)
      ds.put(new Entity(pk2))
      val e1 = new Entity(createKey(pk1, "entity", 1))
      e1.setProperty("name", "e1")
      val e2 = new Entity(createKey(pk2, "entity", 1))
      e2.setProperty("name", "e2")

      ds.put(e1)
      ds.put(e2)

      ds.get(createKey("entity", 1)) must throwA[EntityNotFoundException]
      ds.get(createKey(pk1, "entity", 1)).property[String]("name") must beEqual(Some("e1"))
      ds.get(createKey(pk2, "entity", 1)).property[String]("name") must beEqual(Some("e2"))
    }
  }

}
