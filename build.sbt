name := "scala-atp"

organization := "org.edla"

version := "0.1-SNAPSHOT"

scalaVersion := "2.12.1"

scalacOptions ++= Seq(
  "-language:postfixOps",
  "-language:existentials",
  "-language:implicitConversions",
  //"-optimize",
  "-deprecation",
  "-encoding",
  "UTF-8", // yes, this is 2 args
  "-feature",
  "-unchecked",
  "-Xfatal-warnings",
  "-Xfuture",
  "-Xlint:-nullary-unit", //-nullary-unit required for IntelliJ worksheet
  "-Yno-adapted-args",
  "-Ywarn-dead-code",
  "-Ywarn-numeric-widen",
  "-Ywarn-value-discard"
)

scalacOptions in (Compile, doc) ++= Seq("-diagrams", "-implicits")

libraryDependencies ++= Seq(
  "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.5",
  "org.parboiled"          %% "parboiled"                % "2.1.4",
  "com.lihaoyi"            %% "fastparse"                % "0.4.2",
  "org.scalatest"          %% "scalatest"                % "3.0.1" % "test",
   "org.scala-lang"        %  "scala-reflect"            % "2.12.1" //for lambda.scala
)

//seq(CoverallsPlugin.singleProject: _*)

licenses := Seq("GNU GPL v3" -> url("http://www.gnu.org/licenses/gpl.html"))

homepage := Some(url("http://github.com/newca12/scala-atp"))
