package ya.divaac.res

import org.scalatra.ScalatraServlet
import javax.servlet.http.HttpServletResponse

trait RenderJSON extends ScalatraServlet {

  case class JSON(body: Option[String])

  val CALLBACK_PARAM_PAT = """[a-zA-Z_][a-zA-Z_.0-9]*""".r

  override def inferContentType(actionResult: Any) = actionResult match {
    case _: JSON =>
      params.get("callback") match {
        case Some(_@CALLBACK_PARAM_PAT()) => "text/javascript"
        case _ => "text/json"
      }
    case _ => super.inferContentType(actionResult)
  }

  override def renderResponseBody(actionResult: Any) {
    actionResult match {
      case JSON(Some(body)) =>
        params.get("callback") match {
          case Some(callback@CALLBACK_PARAM_PAT()) =>
            response.getWriter.print(format("%s(%s)", callback, body))
          case _ => response.getWriter.print(body)
        }
      case JSON(None) => status(HttpServletResponse.SC_NO_CONTENT)
      case _ => super.renderResponseBody(actionResult)
    }
  }

}
