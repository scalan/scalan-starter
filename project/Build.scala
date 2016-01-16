import sbt._
import sbt.Keys._

object Build extends Build {
  val commonDeps = libraryDependencies ++= Seq(
    "org.scalatest" %% "scalatest" % "2.2.1" % "test",
    "org.scalacheck" %% "scalacheck" % "1.11.5" % "test")

  val testSettings = inConfig(ItTest)(Defaults.testTasks) ++ Seq(
    // needed thanks to http://stackoverflow.com/questions/7898273/how-to-get-logging-working-in-scala-unit-tests-with-testng-slf4s-and-logback
    parallelExecution in Test := false,
    parallelExecution in ItTest := false,
    publishArtifact in Test := true,
    javaOptions in Test ++= Seq("-Xmx10G", "-Xms5G"),
    publishArtifact in(Test, packageDoc) := false
  )

  val buildSettings = Seq(
    organization := "com.huawei.scalan",
    scalaVersion := "2.11.7",
    scalacOptions ++= Seq(
      "-unchecked", "-deprecation",
      "-feature",
      "-language:higherKinds",
      "-language:implicitConversions",
      "-language:existentials",
      "-language:postfixOps"))

  lazy val noPublishingSettings = Seq(
    publishArtifact := false,
    publish := {},
    publishLocal := {})

  override lazy val settings = super.settings ++ buildSettings

  lazy val commonSettings =
    buildSettings /*++ assemblySettings ++ releaseSettings*/ ++ testSettings ++
      Seq(
      //resolvers += "Scalaz Bintray Repo" at "http://dl.bintray.com/scalaz/releases",
      publishTo := {
        val nexus = "http://10.122.85.37:9081/nexus/"
        if (version.value.trim.endsWith("SNAPSHOT"))
          Some("snapshots" at (nexus + "content/repositories/snapshots"))
        else
          Some("releases" at (nexus + "content/repositories/releases"))
      },
      commonDeps)

  implicit class ProjectExt(p: Project) {
    def allConfigDependency = p % "compile->compile;test->test"

    def addTestConfigsAndCommonSettings =
      p.configs(ItTest).settings(commonSettings: _*)
  }

  def liteProject(name: String) = ProjectRef(file("../scalan-ce"), name)

  def scalanDependency(name: String) = "com.huawei.scalan" %% name % "0.3.0-SNAPSHOT"

  lazy val scalanMeta        = scalanDependency("scalan-meta")
  lazy val scalanCommon      = scalanDependency("scalan-common")
  lazy val scalanCore        = scalanDependency("scalan-core")
  lazy val scalanCollections = scalanDependency("scalan-collections")
  lazy val scalanLA = scalanDependency("scalan-linear-algebra")
  lazy val scalanLms         = scalanDependency("scalan-lms-backend-core")

  lazy val meta = Project(
    id = "scalan-starter-meta",
    base = file("scalan-starter-meta")).addTestConfigsAndCommonSettings.
    settings(fork in run := true, libraryDependencies ++= Seq(scalanMeta))

  lazy val core = Project(
    id = "scalan-starter-core",
    base = file("scalan-starter-core")).addTestConfigsAndCommonSettings.
    settings(libraryDependencies ++= Seq(
      scalanCommon, scalanCommon % "test" classifier "tests",
      scalanCore, scalanCore % "test" classifier "tests",
      scalanCollections, scalanCollections % "test" classifier "tests",
      scalanLA, scalanLA % "test" classifier "tests"
    ))

  val virtScala = Option(System.getenv("SCALA_VIRTUALIZED_VERSION")).getOrElse("2.11.2")

  lazy val lmsBackend = Project(
    id = "scalan-starter-lms-backend",
    base = file("lms-backend")).addTestConfigsAndCommonSettings.
    settings(libraryDependencies ++= Seq(scalanLms),
      scalaOrganization := "org.scala-lang.virtualized",
      scalaVersion := virtScala
    )

  lazy val root = Project(
    id = "scalan-starter",
    base = file(".")).addTestConfigsAndCommonSettings
    .aggregate(meta, core, lmsBackend)
    .settings(
      libraryDependencies ++= Seq(scalanCore, scalanCore % "test" classifier "tests"),
      publishArtifact := false)

  def itFilter(name: String): Boolean =
    name endsWith "ItTests"

  def unitFilter(name: String): Boolean = !itFilter(name)

  lazy val ItTest = config("it").extend(Test)

  publishArtifact in Test := true

  publishArtifact in packageDoc := !version.value.trim.endsWith("SNAPSHOT")

  publishTo in ThisBuild := {
    val nexus = "http://10.122.85.37:9081/nexus/"
    if (version.value.trim.endsWith("SNAPSHOT"))
      Some("snapshots" at (nexus + "content/repositories/snapshots"))
    else
      Some("releases" at (nexus + "content/repositories/releases"))
  }
}
