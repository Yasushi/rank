package ya.divaac

import org.specs._
import java.util.{Calendar, Date, TimeZone}
import java.text.SimpleDateFormat
import Calendar._

class DateUtilsSpecs extends Specification {

  "CalendarW" should {
    val c = Calendar.getInstance
    val w = new CalendarW(c)

    def newCalendar = {
      val c = Calendar.getInstance
      c.set(YEAR, 2010)
      c.set(MONTH, SEPTEMBER)
      c.set(DATE, 5)
      c.set(HOUR_OF_DAY, 14)
      c.set(MINUTE, 30)
      c.set(SECOND, 30)
      c.set(MILLISECOND, 30)
      c
    }

    val fields =
      List(MONTH, DATE, HOUR_OF_DAY, HOUR, MINUTE, SECOND, MILLISECOND)
    "apply" in {
     fields.foreach(f => w(f) must beEqual(c.get(f)))
    }
    "update" in {
      for (field <- fields) {
        val w = new CalendarW(newCalendar)
        w(field) = 1
        w(field) must beEqual(1)
      }
    }
    "add" in {
      for (field <- fields) {
        val w = new CalendarW(newCalendar)
        val before = w(field)
        w+=(field, 1)
        w(field) must beEqual(before + 1)
      }
    }
    "sub" in {
      for (field <- fields) {
        val w = new CalendarW(newCalendar)
        val before = w(field)
        w-=(field, 1)
        w(field) must beEqual(before - 1)
      }
    }
    "getAll" in {
      new CalendarW(newCalendar).getAll must beLike {
        case (2010, 9, 5, 14, 30, 30, 30) => true
      }
    }
    "setAll" in {
      def nw = new CalendarW(newCalendar)
      nw.setAll(2009, 8, 4, 10, 20, 40, 50).getAll must beLike {
        case (2009, 8, 4, 10, 20, 40, 50) => true
      }
      nw.setAll(2009, 8, 4, 10, 20, 40).getAll must beLike {
        case (2009, 8, 4, 10, 20, 40, 0) => true
      }
      nw.setAll(2009, 8, 4, 10, 20).getAll must beLike {
        case (2009, 8, 4, 10, 20, 0, 0) => true
      }
      nw.setAll(2009, 8, 4, 10).getAll must beLike {
        case (2009, 8, 4, 10, 0, 0, 0) => true
      }
      nw.setAll(2009, 8, 4).getAll must beLike {
        case (2009, 8, 4, 0, 0, 0, 0) => true
      }
      nw.setAll(2009, 8).getAll must beLike {
        case (2009, 8, 5, 0, 0, 0, 0) => true
      }
      nw.setAll(2009).getAll must beLike {
        case (2009, 9, 5, 0, 0, 0, 0) => true
      }
      nw.setAll().getAll must beLike {
        case (2010, 9, 5, 0, 0, 0, 0) => true
      }
    }
  }

  "DateUtils" should {
    "asCalendar" in {
      import DateUtils._
      asCalendar(2010, 9, 5, 14, 30, 30, 30).getAll must beLike {
        case (2010, 9, 5, 14, 30, 30, 30) => true
      }
    }
    "rankingId" in {
      import DateUtils._
      implicit def cal2date(c: Calendar) = c.getTime
      rankingId(asCalendar(2010, 9, 5, 14, 30)) must beEqual("20100905")
      rankingId(asCalendar(2010, 9, 5, 12, 00)) must beEqual("20100905")
      rankingId(asCalendar(2010, 9, 5, 11, 59)) must beEqual("20100904")
      rankingId(asCalendar(2010, 9, 5,  0,  0)) must beEqual("20100904")
    }
  }
}
