import sbt.{Compile, ThisBuild, file, project}

import scala.collection.Seq

ThisBuild / scalaVersion := "3.3.5"

val gbgeCommitHash = "eb7110edc8c1cc8f2d17e3161ef2fa7a724d1954"

lazy val common = crossProject(JSPlatform, JVMPlatform).in(file("common")).
  settings(
    name := "common",
    libraryDependencies ++= Seq(
      "com.github.kurgansoft" % "gbge" % gbgeCommitHash,
      "dev.optics" %%% "monocle-core" % "3.3.0",
      "dev.optics" %%% "monocle-macro" % "3.3.0",
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
    "org.scalatest" %% "scalatest" % "3.2.20" % Test
  ),
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
