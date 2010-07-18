import sbt._

class DiivaacRank(info: ProjectInfo) extends AppengineProject(info) {
  val tagsoup = "org.ccil.cowan.tagsoup" % "tagsoup" % "1.2"

  val specs = "org.scala-tools.testing" %% "specs" % "1.6.5-SNAPSHOT" % "test"

  val sage = "nkpart" % "sage_2.8.0.RC7" % "0.1"
  val scalaToolsSnapshot = ScalaToolsSnapshots
}
