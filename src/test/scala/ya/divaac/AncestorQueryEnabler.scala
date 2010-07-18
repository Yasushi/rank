package ya.divaac

// Kindless Ancestor Queryをローカル環境で使用する #appengine
// http://shin1o.blogspot.com/2010/01/kindless-ancestor-query-appengine.html
// for 1.3.5

import com.google.appengine.tools.development._
import com.google.apphosting.api.ApiProxy
import com.google.apphosting.api.ApiProxy.ApiConfig
import com.google.apphosting.api.ApiProxy.Environment
import com.google.apphosting.api.ApiProxy.LogRecord
import com.google.apphosting.api.DatastorePb
import com.google.apphosting.api.DatastorePb.GetSchemaRequest
import com.google.apphosting.api.DatastorePb.Schema

import scala.collection.JavaConversions._

class AncestorQueryEnabler extends ApiProxyLocal {
  assert(!AppengineUtils.isProduction)
  val before = ApiProxy.getDelegate.asInstanceOf[ApiProxyLocal]

  def setProperty(key: String, value: String) = before.setProperty(key, value)
  def setProperties(paramMap: java.util.Map[String, String]) = before.setProperties(paramMap)
  def stop = before.stop
  def getService(name: String) = before.getService(name)
  def getClock = before.getClock
  def setClock(clock: Clock) = before.setClock(clock)
  def makeAsyncCall(env: Environment, service: String, method: String, request: Array[Byte], config: ApiConfig) =
    before.makeAsyncCall(env, service, method, request, config)
  def log(env: Environment, logRecord: LogRecord) = before.log(env, logRecord)

  def makeSyncCall(env: Environment, service: String, method: String, request: Array[Byte]): Array[Byte] = {
    def beforeMakeSyncCall =
      before.makeSyncCall(env, service, method, request)
    if (service != "datastore_v3" || method != "RunQuery")
      return beforeMakeSyncCall

    // kindless ancestor queryかどうかをチェックするために、pbを組立て直す。
    val requestPb = new DatastorePb.Query
    requestPb.mergeFrom(request)
    if (Option(requestPb.getAncestor).isEmpty || !requestPb.getKind.isEmpty)
      return beforeMakeSyncCall

    // kindless ancestor queryの時は全てのKindに対してancestor queryを実行する
    val kinds = getKinds(env)
    // ancestor keyのpbからancestorのkindを取得し、最初はancestor kindから実行する。
    val ancestorKind = requestPb.getAncestor.getPath.getElement(0).getType
    val resultOfAncestor =
      runAncestorQuery(env, service, method, ancestorKind, requestPb)
    resultOfAncestor.setMoreResults(true)
    for (kind <- kinds if kind != ancestorKind) {
      // ancestor kind以外の全てのkindに対してancestorクエリを実行する
      val result = runAncestorQuery(env, service, method, kind, requestPb)
      result.resultIterator.foreach{i => resultOfAncestor.addResult(i)}
    }
    return resultOfAncestor.toByteArray
  }

  def runAncestorQuery(env: Environment, service: String, method: String, kind: String, requestPb: DatastorePb.Query) = {
    requestPb.setKind(kind)
    val requestbytes = requestPb.toByteArray
    val result = new DatastorePb.QueryResult
    result.mergeFrom(before.makeSyncCall(env, service, method, requestbytes))
    result
  }

  def getKinds(env: Environment) = {
    val req = new GetSchemaRequest
    req.setApp(env.getAppId)
    val resBuf = before.makeSyncCall(env, "datastore_v3", "GetSchema", req.toByteArray)
    val schema = new Schema
    schema.mergeFrom(resBuf)
    schema.kinds.map(_.getKey.getPath.elements.last.getType).toArray
  }

}
