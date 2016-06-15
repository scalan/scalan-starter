import sbt._
import sbt.Keys._

object Build extends Build {
  val testSettings = Defaults.itSettings ++ Seq(
    // needed thanks to http://stackoverflow.com/questions/7898273/how-to-get-logging-working-in-scala-unit-tests-with-testng-slf4s-and-logback
    parallelExecution in Test := false,
    parallelExecution in IntegrationTest := false,
    javaOptions in Test ++= Seq("-Xmx10G", "-Xms5G"),
    javaOptions in IntegrationTest ++= Seq("-Xmx10G", "-Xms5G"),
    libraryDependencies ++= Seq("org.scalatest" %% "scalatest" % "2.2.6" % "test;it")
  )

  lazy val commonSettings =
    Seq(
      organization := "com.huawei.scalan",
      scalaVersion := "2.11.8",
      scalacOptions ++= Seq(
        "-unchecked", "-deprecation", "-feature",
        "-language:higherKinds", "-language.existentials"
      )
    ) ++ testSettings /*++ assemblySettings */

  implicit class ProjectExt(p: Project) {
    def allConfigDependency = p % "compile->compile;test->test"

    def addTestConfigsAndCommonSettings =
      p.configs(IntegrationTest).settings(commonSettings: _*)
  }

  def scalanDependency(name: String) = "com.huawei.scalan" %% name % "0.3.0-SNAPSHOT"

  lazy val scalanMeta        = scalanDependency("scalan-meta")
  lazy val scalanCommon      = scalanDependency("scalan-common")
  lazy val scalanCore        = scalanDependency("scalan-core")
  lazy val scalanCollections = scalanDependency("scalan-collections")
  lazy val scalanEffects     = scalanDependency("scalan-effects")
  lazy val scalanLA = scalanDependency("scalan-linear-algebra")

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
      scalanEffects, scalanEffects % "test" classifier "tests",
      scalanLA, scalanLA % "test" classifier "tests"
    ), fork := true)

  lazy val root = Project(
    id = "scalan-starter",
    base = file(".")).addTestConfigsAndCommonSettings
    .aggregate(meta, core)
    .settings(publishArtifact := false)

  publishArtifact in packageDoc := !version.value.trim.endsWith("SNAPSHOT")

  publishTo in ThisBuild := {
    val nexus = "http://10.122.85.37:9081/nexus/"
    if (version.value.trim.endsWith("SNAPSHOT"))
      Some("snapshots" at (nexus + "content/repositories/snapshots"))
    else
      Some("releases" at (nexus + "content/repositories/releases"))
  }
}
