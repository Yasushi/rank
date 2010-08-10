package ya.divaac

import java.util.{Calendar, Date, TimeZone}
import java.text.SimpleDateFormat

class CalendarW(val c: Calendar = Calendar.getInstance) {
  import Calendar._
  c.setTimeZone(DateUtils.JST)

  def +=(a: Tuple2[Int, Int]) = c.add(a._1, a._2)
  def -=(a: Tuple2[Int, Int]) = c.add(a._1, -1 * a._2)
  def apply(field: Int) = c.get(field)
  def update(field: Int, amount: Int) = c.set(field, amount)
  def getAll =
    (this(YEAR), this(MONTH) + 1, this(DATE), this(HOUR_OF_DAY), this(MINUTE), this(SECOND), this(MILLISECOND))
  def setAll(amounts: Int*) = {
    amounts.drop(0).headOption.foreach(a => update(YEAR, a))
    amounts.drop(1).headOption.filter((1 to 12).contains).foreach(a => update(MONTH, a - 1))
    amounts.drop(2).headOption.filter((1 to 31).contains).foreach(a => update(DATE, a))
    amounts.drop(3).headOption.filter((0 to 23).contains).orElse(Some(0)).foreach(a => update(HOUR_OF_DAY, a))
    amounts.drop(4).headOption.filter((0 to 60).contains).orElse(Some(0)).foreach(a => update(MINUTE, a))
    amounts.drop(5).headOption.filter((0 to 60).contains).orElse(Some(0)).foreach(a => update(SECOND, a))
    amounts.drop(6).headOption.orElse(Some(0)).foreach(a => update(MILLISECOND, a))
    this
  }
  def setTZ(tz: TimeZone) = {
    c.setTimeZone(tz)
    this
  }
  def setDate(d: Date) = {
    c.setTime(d)
    this
  }
  def truncate = {
    update(HOUR_OF_DAY, 0)
    update(MINUTE, 0)
    update(SECOND, 0)
    update(MILLISECOND, 0)
    this
  }

  def normalize() = {
    c.add(Calendar.HOUR_OF_DAY, -12)
    truncate
  }
}

object DateUtils {
  val JST = TimeZone.getTimeZone("JST")
  implicit def asCalendarW(c: Calendar) = new CalendarW(c)
  implicit def asCalendar(c: CalendarW) = c.c
  implicit def asCalendar(d: Date): Calendar =
    Calendar.getInstance.setTZ(JST).setDate(d)

  val RANKING_DATE_PAT = """(\d{4})(\d{2})(\d{2})""".r

  def asCalendar(amounts: Int*): Calendar =
    Calendar.getInstance.setTZ(JST).setAll(amounts:_*)

  def rankingId(d: Date = new Date) = rankingDate(d)
  def rankingDate(d: Date = new Date) = {
    val c = asCalendar(d)
    c.normalize()
    format("%tY%<tm%<td", c)
  }
  def parseRankingDate(s: String) = {
    s match {
      case RANKING_DATE_PAT(y, m, d) => Some(asCalendar(y.toInt, m.toInt, d.toInt, 12).getTime)
      case _ => None
    }
  }

  def range(d: Date = new Date): (Date, Date) = {
    val c = asCalendar(d)
    c.normalize()
    c += (Calendar.HOUR_OF_DAY, 12)
    val start = c.getTime
    c += (Calendar.DATE, 1)
    val end = c.getTime
    (start, end)
  }

}
