import sbt._
import sbt.Keys._
//import sbtassembly.Plugin._
//import AssemblyKeys._
//import sbtrelease.ReleasePlugin._

object ScalanStartRootBuild extends Build {
  val commonDeps = libraryDependencies ++= Seq(
    "org.scalaz.stream" %% "scalaz-stream" % "0.6a",
    //"junit" % "junit" % "4.11" % "test",
    //("com.novocode" % "junit-interface" % "0.11" % "test").exclude("junit", "junit-dep").exclude("org.scala-tools.testing", "test-interface"),
    "org.scalatest" %% "scalatest" % "2.2.1" % "test",
    "org.scalacheck" %% "scalacheck" % "1.11.5" % "test")

  val testSettings = inConfig(ItTest)(Defaults.testTasks /*++ baseAssemblySettings*/) ++ Seq(
    testOptions in Test := Seq(Tests.Argument(TestFrameworks.JUnit, "-a", "-s"), Tests.Filter(unitFilter)),
    testOptions in ItTest := Seq(Tests.Argument(TestFrameworks.JUnit, "-v", "-a", "-s", "-q"), Tests.Filter(itFilter)),
    // needed thanks to http://stackoverflow.com/questions/7898273/how-to-get-logging-working-in-scala-unit-tests-with-testng-slf4s-and-logback
    parallelExecution in Test := false,
    parallelExecution in ItTest := false,
    publishArtifact in Test := true,
    publishArtifact in(Test, packageDoc) := false
    //jarName in(ItTest, assembly) := s"${name.value}-test-${version.value}.jar"
    )

  val buildSettings = Seq(
    organization := "com.huawei.scalan",
    scalaVersion := "2.10.4",
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
      resolvers += "Scalaz Bintray Repo" at "http://dl.bintray.com/scalaz/releases",
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

  def liteProject(name: String) = ProjectRef(file("../scalan-lite"), name)

  def liteDependency(name: String) = "com.huawei.scalan" %% name % "0.2.3"

//  lazy val all = project.aggregate(start).addTestConfigsAndCommonSettings.
//    settings(noPublishingSettings: _*)

  lazy val start = Project(
    id = "scalan-start",
    base = file(".")).addTestConfigsAndCommonSettings.
    settings(libraryDependencies ++= Seq(core, core % "test" classifier "tests"))

//  lazy val examples = Project("enterprise-edition-examples", file("examples")).
//    dependsOn(enterprise.allConfigDependency).addTestConfigsAndCommonSettings
//
//  lazy val meta = Project("enterprise-edition-meta", file("meta")).addTestConfigsAndCommonSettings
//    .settings(noPublishingSettings: _*)
//    .settings(
//      fork in run := true,
//      libraryDependencies += liteDependency("meta"))

  // lazy val core = liteProject("core").allConfigs
  lazy val core = liteDependency("core")

  def itFilter(name: String): Boolean =
    name endsWith "ItTests"

  def unitFilter(name: String): Boolean = !itFilter(name)

  lazy val ItTest = config("it").extend(Test)

  // lazy val community = project.aggregate(core, liteProject("community-edition"), liteProject("lms-backend"))

  publishArtifact in Test := true

  publishArtifact in (Test, packageDoc) := false

  publishTo in ThisBuild := {
    val nexus = "http://10.122.85.37:9081/nexus/"
    if (version.value.trim.endsWith("SNAPSHOT"))
      Some("snapshots" at (nexus + "content/repositories/snapshots"))
    else
      Some("releases" at (nexus + "content/repositories/releases"))
  }
}
