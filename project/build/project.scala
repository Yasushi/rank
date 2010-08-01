import sbt._

class DiivaacRank(info: ProjectInfo) extends AppengineProject(info) with JRebel {
  val tagsoup = "org.ccil.cowan.tagsoup" % "tagsoup" % "1.2"
  val slf4j = "org.slf4j" % "slf4j-jdk14" % "1.6.1"
  val labs = AppengineApiLabsJar

  val specs = "org.scala-tools.testing" %% "specs" % "1.6.5-SNAPSHOT" % "test"

  val sage = "nkpart" %% "sage" % "0.1"
  val scalaToolsSnapshot = ScalaToolsSnapshots

  override def jrebelJvmOptions =
    List("-Drebel.log=true", "-Drebel.log.stdout=true")
}
