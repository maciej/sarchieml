organization := "me.maciejb.sarchieml"
name := "sarchieml"
version := "1.0"
scalaVersion := "2.11.7"

scalacOptions ++= Seq("-feature", "-unchecked", "-deprecation", "-encoding", "utf8")

val fastparse = "com.lihaoyi" %% "fastparse" % "0.2.1"
val reflect = "org.scala-lang" % "scala-reflect"
val sprayJson = "io.spray" %% "spray-json" % "1.3.2"
val commonsIo = "commons-io" % "commons-io" % "2.4"

val scalatest = "org.scalatest" %% "scalatest" % "2.2.4" % "test"

libraryDependencies ++= Seq(scalatest, fastparse, reflect % scalaVersion.value, sprayJson,
  commonsIo % "test")
