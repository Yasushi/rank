package ya.divaac

import scala.util.Properties._

import com.google.appengine.api.datastore._
import com.google.appengine.api.memcache._
import com.google.apphosting.api._
import DatastorePb.{GetSchemaRequest, PutRequest, Schema}
import com.google.storage.onestore.v3.OnestoreEntity.Reference

import scala.collection.JavaConversions._

object AppengineUtils {
  val ENVIRONMENT = "com.google.appengine.runtime.environment"
  val VERSION = "com.google.appengine.runtime.version"

  lazy val isServer = propIsSet(ENVIRONMENT)
  lazy val isDevelopment = propOrNone(ENVIRONMENT) exists(_ == "Development")
  lazy val isProduction = propOrNone(ENVIRONMENT) exists(_ == "Production")
  lazy val version = propOrEmpty(VERSION)

  object Datastore {
    def getSchema = {
      if (isProduction)
        throw new IllegalStateException("")
      val req = new GetSchemaRequest
      req.setApp(ApiProxy.getCurrentEnvironment.getAppId)
      val resBuf = ApiProxy.makeSyncCall("datastore_v3", "GetSchema", req.toByteArray)
      val schema = new Schema
      schema.mergeFrom(resBuf)
      schema
    }

    def getKinds = {
      if (isProduction)
        throw new IllegalStateException("")
      getSchema.kinds.map(_.getKey).map(getKind).toList
    }

    def getKind(key: Reference) = key.getPath.elements.last.getType
  }

  object Memcache {
    val memcacheService = MemcacheServiceFactory.getMemcacheService
    import Expiration._
    val defaultExpire = byDeltaSeconds(3600)

    def validValue[R](value: R) = value != null && (value match {
      case s: String => !s.isEmpty
      case s: Seq[_] => !s.isEmpty
      case s: Array[_] => !s.isEmpty
      case _ => true
    })
    class Memoize1[-T, +R](f: T => R, expire: Expiration) extends (T => R) {
      def apply(x: T): R ={
        Option(memcacheService.get(x)) match {
          case None => {
            val value = f(x)
            if (validValue(value))
              memcacheService.put(x, value, expire)
            value
          }
          case Some(value) => value.asInstanceOf[R]
        }
      }
    }
    object Memoize1 {
      def apply[T, R](f: T => R, expire: Expiration) = new Memoize1(f, expire)
      def apply[T, R](f: T => R, expire: Int) = new Memoize1(f, byDeltaSeconds(expire))
      def apply[T, R](f: T => R) = new Memoize1(f, defaultExpire)
    }
    class Memoize0[-T, +R](f: => R, key: T, expire: Expiration) extends (() => R) {
      def apply(): R ={
        Option(memcacheService.get(key)) match {
          case None => {
            val value = f
            if (validValue(value))
              memcacheService.put(key, value, expire)
            value
          }
          case Some(value) => value.asInstanceOf[R]
        }
      }
    }
    object Memoize0 {
      def apply[T, R](f: => R, key: T, expire: Expiration) = new Memoize0(f, key, expire)
      def apply[T, R](f: => R, key: T, expire: Int) = new Memoize0(f, key, byDeltaSeconds(expire))
      def apply[T, R](f: => R, key: T) = new Memoize0(f, key, defaultExpire)
    }

  }
}
