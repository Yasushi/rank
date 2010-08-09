// http://gist.github.com/381978

package ya.divaac

import java.io._
object JSONLiteral {
  sealed trait JSONValue
  case class JSONObject(members: Map[String, JSONValue]) extends JSONValue
  case class JSONString(value: String) extends JSONValue
  case class JSONNumber(value: Double) extends JSONValue
  case class JSONArray(members: List[JSONValue]) extends JSONValue
  implicit def int2JSONNumber(value: Int) = JSONNumber(value)
  implicit def double2JSONNumber(value: Double) = JSONNumber(value)
  implicit def string2JSONString(value: String) = JSONString(value)
  def O(members: (String, JSONValue)*): JSONObject = JSONObject(Map(members:_*))
  def A(members: JSONValue*): JSONArray = JSONArray(members.toList)

  def using[A <: { def close() }, B](resource: A)(f: A => B): B = {
    try {
      f(resource)
    } finally {
      resource.close()
    }
  }
  def dump(jsonObject: JSONObject, to: File, encoding: String) {
    using(new BufferedWriter(
      new OutputStreamWriter(new FileOutputStream(to), encoding))){out =>
        out.write(toString(jsonObject))
                                                                 }
  }
  private val escapeMap = Map(
    '\r' -> 'r', '\n' -> 'n', '\t' -> 't', '\f' -> 'f',
    '\\' -> '\\', '\'' -> '\'', '\"' -> '\"', '\b' -> 'b')
  def toString(jsonObject: JSONValue): String = {
    def escape(content: String): String = {
      content.foldLeft(new java.lang.StringBuilder){(buf, ch) =>
        ch match {
          case ch if escapeMap.contains(ch) =>
            buf.append("\\"); buf.append(escapeMap(ch))
          case ch => buf.append(ch)
        }
      }.toString
    }
    jsonObject match {
      case JSONObject(members) =>
        members.map{ case (k, v) => "\"" + escape(k) + "\":" + toString(v) }
      .mkString("{", ",", "}")
      case JSONString(value) => "\"" + escape(value) + "\""
      case JSONArray(members) => members.map(toString(_)).mkString("[", ",", "]")
      case JSONNumber(value) => value.toString
    }
  }
}
