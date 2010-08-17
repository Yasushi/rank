package ya.divaac.res

import org.scalatra.ScalatraServlet
import javax.servlet.http.HttpServletResponse

import ya.divaac._

abstract class BaseServlet extends ScalatraServlet with RenderJSON with Log {
  notFound {
    status(404)
    if (AppengineUtils.isProduction) {
      info("Requesting {} but only have {}", request.getRequestURI, Routes)
      response.setContentType("text/html")
      response.getWriter println """<html><head>
<meta http-equiv="content-type" content="text/html;charset=utf-8">
<title>404 NOT_FOUND</title>
</head>
<body text=#000000 bgcolor=#ffffff>
<h1>Error: NOT_FOUND</h1>
</body></html>
"""
    } else {
      response.getWriter println "Requesting %s but only have %s".format(request.getRequestURI, Routes)
    }
  }

  error {
    error("error", caughtThrowable)
    response.getWriter println("error " + caughtThrowable.getMessage)
  }

  override def halt(code: Int, msg: String) = {
    info("halt %03d %s".format(code, msg))
    super.halt(code, msg)
  }

}
