// http://johlrogge.wordpress.com/2009/06/27/loggingtools-in-scala/

package ya.divaac

import org.slf4j.LoggerFactory

trait Log {
  private val log = LoggerFactory.getLogger(getClass)

  def trace(message: String, values: Any*) =
    log.trace(message, values.map(_.asInstanceOf[Object]).toArray)
  def trace(message: String, error: Throwable) = log.trace(message, error)

  def debug(message: String, values: Any*) =
    log.debug(message, values.map(_.asInstanceOf[Object]).toArray)
  def debug(message: String, error: Throwable) = log.debug(message, error)

  def info(message: String, values: Any*) =
    log.info(message, values.map(_.asInstanceOf[Object]).toArray)
  def info(message: String, error: Throwable) = log.info(message, error)

  def warn(message: String, values: Any*) =
    log.warn(message, values.map(_.asInstanceOf[Object]).toArray)
  def warn(message: String, error: Throwable) = log.warn(message, error)

  def error(message: String, values: Any*) =
    log.error(message, values.map(_.asInstanceOf[Object]).toArray)
  def error(message: String, error: Throwable) = log.error(message, error)
}
