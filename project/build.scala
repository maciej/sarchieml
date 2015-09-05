import bintray.BintrayPlugin.autoImport._
import sbt.Keys._
import sbt._

object Settings {
  val publishing: Seq[Setting[_]] = Seq(
    isSnapshot <<= isSnapshot or version(_ endsWith "-SNAPSHOT"),
    publishMavenStyle := true,
    publishArtifact in Test := false,
    pomExtra :=
      <developers>
        <developer>
          <id>maciej</id>
          <name>Maciej Bilas</name>
          <url>http://twitter.com/maciejb</url>
        </developer>
      </developers>,
    licenses := ("Apache-2.0", new java.net.URL("http://www.apache.org/licenses/LICENSE-2.0.txt")) :: Nil,
    homepage := Some(new java.net.URL("http://maciejb.me")),
    bintrayOrganization := Some("maciej")
  )
}