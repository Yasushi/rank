package ya.divaac

import javax.servlet.http._

object Utils {

  def printJSON(json: Option[String],
                req: HttpServletRequest, resp: HttpServletResponse) {
    json match {
      case None =>
        resp.setStatus(HttpServletResponse.SC_NO_CONTENT)
      case Some(j) =>
        Option(req.getParameter("callback")) match {
          case None => {
            resp.setContentType("text/json")
            resp.setCharacterEncoding("UTF-8")
            resp.getWriter.print(j)
          }
          case Some(callback) => {
            resp.setContentType("text/javascript")
            resp.setCharacterEncoding("UTF-8")
            resp.getWriter.print(format("%s(%s)", callback, j))
          }
        }
        resp.getWriter.flush
        resp.getWriter.close
    }
  }

}
