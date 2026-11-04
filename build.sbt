import sbt.{Compile, ThisBuild, file, project}

import scala.collection.Seq

ThisBuild / scalaVersion := "3.9.0"
ThisBuild / scalacOptions ++= Seq("-experimental")

val gbgeCommitHash = "c9bf42b0a0605050c313107c972bfe1add0ad2bd"

lazy val common = crossProject(JSPlatform, JVMPlatform).in(file("common")).
  settings(
    name := "common",
    libraryDependencies ++= Seq(
      "com.github.kurgansoft" % "gbge" % gbgeCommitHash,
    ),
    resolvers += "jitpack" at "https://jitpack.io"
  )
  .jvmSettings(
    target := file("target/jvm")
  )
  .jsSettings(
    target := file("target/js"),
    scalaJSUseMainModuleInitializer := false
  )

lazy val backend = project.in(file("backend")).settings(
  name := "backend",
  mainClass := Some("risk.backend.launchers.Launcher"),
  libraryDependencies ++= Seq(
    "dev.zio" %% "zio-test" % "2.1.27" % Test,
    "dev.zio" %% "zio-test-sbt" % "2.1.27" % Test
  ),
  testFrameworks += new TestFramework("zio.test.sbt.ZTestFramework"),
  assemblyMergeStrategy := {
    case x if Assembly.isConfigFile(x) =>
      MergeStrategy.concat
    case PathList(ps @ _*) if Assembly.isReadme(ps.last) || Assembly.isLicenseFile(ps.last) =>
      MergeStrategy.rename
    case PathList("META-INF", xs @ _*) =>
      xs map {_.toLowerCase} match {
        case "manifest.mf" :: Nil | "index.list" :: Nil | "dependencies" :: Nil =>
          MergeStrategy.discard
        case ps @ x :: xs if ps.last.endsWith(".sf") || ps.last.endsWith(".dsa") =>
          MergeStrategy.discard
        case "plexus" :: xs =>
          MergeStrategy.discard
        case "services" :: xs =>
          MergeStrategy.filterDistinctLines
        case "spring.schemas" :: Nil | "spring.handlers" :: Nil =>
          MergeStrategy.filterDistinctLines
        case _ => MergeStrategy.first
      }
    case other if other.endsWith(".sjsir") || other.contains("_sjs1") =>
      MergeStrategy.discard
    case _ => MergeStrategy.first
  }
).dependsOn(common.jvm)

lazy val ui = project.in(file("ui")).settings(
  name := "ui",
  Compile / scalaJSUseMainModuleInitializer:= false,
//  scalacOptions ++= Seq(
//    "-Xmax-inlines", "100"
//  ),
  resolvers += "jitpack" at "https://jitpack.io"
).dependsOn(common.js).enablePlugins(ScalaJSPlugin)
