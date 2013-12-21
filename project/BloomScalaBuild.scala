import sbt._
import sbt.Keys._

object BloomScalaBuild extends Build {

  lazy val bloomScala = Project(
    id = "bloom-scala",
    base = file("."),
    settings = Project.defaultSettings ++ Seq(
      name := "bloom-scala",
      //organization := "edu.berkeley.cs.amplab",
      version := "0.1-SNAPSHOT",
      scalaVersion := "2.10.3",
      resolvers ++= Seq(
        "sonatype-snapshots" at "http://oss.sonatype.org/content/repositories/snapshots",
        "sonatype-releases"  at "http://oss.sonatype.org/content/repositories/releases",
        "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/",
        "JBoss Repository" at "http://repository.jboss.org/nexus/content/repositories/releases/"
      ),
      libraryDependencies ++= Seq(
        "org.scalaz" %% "scalaz-core" % "7.0.5",
        "com.typesafe" %% "scalalogging-slf4j" % "1.0.1",
        "org.slf4j" % "slf4j-log4j12" % "1.7.5"
      )
    )
  )
}
