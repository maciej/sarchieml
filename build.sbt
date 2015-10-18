organization := "me.maciejb.sarchieml"
name := "sarchieml"
description := "A Scala parser for ArchieML"
homepage := Some(url("https://github.com/maciej/sarchieml"))
startYear := Some(2015)
licenses := Seq("Apache-2.0" -> url("http://www.apache.org/licenses/LICENSE-2.0.txt"))

pomIncludeRepository := { _ => false }
publishMavenStyle := true
//noinspection ScalaUnnecessaryParentheses
pomExtra := (
  <scm>
    <url>git@github.com:maciej/sarchieml.git</url>
    <connection>scm:git:git@github.com:maciej/sarchieml.git</connection>
  </scm>
    <developers>
      <developer>
        <id>maciej</id>
        <name>Maciej Bilas</name>
        <url>http://maciejb.me</url>
      </developer>
    </developers>
  )

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
