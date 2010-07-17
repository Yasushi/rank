package ya.divaac

import scala.util.Properties._

import com.google.appengine.api.datastore._
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
}
