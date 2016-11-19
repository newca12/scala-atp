name := "scala-atp"

organization := "org.edla"

version := "0.1-SNAPSHOT"

scalaVersion := "2.11.7"

scalacOptions ++= Seq(
  "-language:postfixOps", "-language:existentials", "-language:implicitConversions",
  //"-optimize",
  "-deprecation",
  "-encoding", "UTF-8", // yes, this is 2 args
  "-feature",
  "-unchecked",
  "-Xfatal-warnings",
  "-Xlint",
  "-Yno-adapted-args",
  "-Ywarn-numeric-widen",
  "-Ywarn-value-discard",
  "-Xfuture"
)
scalacOptions in (Compile, doc) ++= Seq("-diagrams","-implicits")

resolvers += "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots"

libraryDependencies ++= Seq(
  //"at.logic.gapt" %% "gapt" % "1.10-SNAPSHOT",
  "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.4",
  //workaround for ivy : impossible to get artifacts when data has not been loaded.
  //http://harrah.github.io/xsbt/latest/sxr/Ivy.scala.html
  //"org.scala-lang.modules" %% "scala-xml" % "1.0.4",
  "org.parboiled" %% "parboiled" % "2.2.0-SNAPSHOT",
  "com.lihaoyi" %% "fastparse" % "0.3.4",
  "org.scalatest" %% "scalatest" % "2.2.5" % "test",
  "com.twitter" % "util-core_2.11" % "6.24.0"
)

//seq(CoverallsPlugin.singleProject: _*)

// Uncomment the following line to use one-jar (https://github.com/sbt/sbt-onejar)
//seq(com.github.retronym.SbtOneJar.oneJarSettings: _*)

licenses := Seq("GNU GPL v3" -> url("http://www.gnu.org/licenses/gpl.html"))

homepage := Some(url("http://github.com/newca12/scala-atp"))
