import sbt._

class DiivaacRank(info: ProjectInfo) extends AppengineProject(info) with JRebel {
  val tagsoup = "org.ccil.cowan.tagsoup" % "tagsoup" % "1.2"
  val slf4j = "org.slf4j" % "slf4j-jdk14" % "1.6.1"
  val labs = AppengineApiLabsJar
  val scalatra = "org.scalatra" %% "scalatra" % "2.0.0.M1"

  val specs = "org.scala-tools.testing" %% "specs" % "1.6.5" % "test"
  val scalacheck = "org.scala-tools.testing" %% "scalacheck" % "1.7" % "test"

  val sage = "nkpart" %% "sage" % "0.1"
  val scalaToolsSnapshot = ScalaToolsSnapshots

  lazy val resolve = task {
    updateIvyModule.withModule{ (i,m,c) =>
      i.resolve(m, new org.apache.ivy.core.resolve.ResolveOptions) }
    None
  }

  override def jrebelJvmOptions =
    List("-Drebel.log=true", "-Drebel.log.stdout=true")

  override def consoleInit = """
import com.google.appengine.tools.development.testing._
import ya.divaac._
val helper = new LocalServiceTestHelper(
  new LocalMemcacheServiceTestConfig()
)
helper.setUp
"""
}
