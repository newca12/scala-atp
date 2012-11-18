name := "scala-atp"

organization := "org.edla"

version := "0.1-SNAPSHOT"

scalaVersion := "2.10.0-RC2"
//scalaVersion := "2.9.2"

libraryDependencies ++= Seq(
  "junit" % "junit" % "4.8.2",
  "org.specs2" % "specs2_2.10.0-RC2" % "1.12.2" % "test"
  //"org.specs2" % "specs2_2.9.2" % "1.12.2" % "test"
)