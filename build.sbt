organization := "me.maciejb.sarchieml"
name := "sarchieml"
scalaVersion := "2.11.7"

val fastparse = "com.lihaoyi" %% "fastparse" % "0.2.1"
val reflect = "org.scala-lang" % "scala-reflect"
val sprayJson = "io.spray" %% "spray-json" % "1.3.2"
val commonsIo = "commons-io" % "commons-io" % "2.4"

val scalatest = "org.scalatest" %% "scalatest" % "2.2.4" % "test"

val sarchieml = project.in(file("."))
  .settings(scalacOptions ++= Seq("-feature", "-unchecked", "-deprecation", "-encoding", "utf8"))
  .settings(libraryDependencies ++= Seq(scalatest, fastparse, reflect % scalaVersion.value, sprayJson,
  commonsIo % "test"))
  .settings(Settings.publishing: _*)
