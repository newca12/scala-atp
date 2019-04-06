enablePlugins(GraalVMNativeImagePlugin)
//sbt 'show graalvm-native-image:packageBin'
enablePlugins(ScalaNativePlugin)

name := "scala-atp"
organization := "org.edla"
version := "0.5"

scalaVersion in ThisBuild := "2.11.12"

mainClass in Compile := Some("org.edla.port.atp.Main")

scalacOptions ++= Seq(
  "-deprecation", // Emit warning and location for usages of deprecated APIs.
  "-encoding",
  "utf-8", // Specify character encoding used by source files.
  "-explaintypes", // Explain type errors in more detail.
  "-feature", // Emit warning and location for usages of features that should be imported explicitly.
  "-language:existentials", // Existential types (besides wildcard types) can be written and inferred
  "-language:experimental.macros", // Allow macro definition (besides implementation and application)
  "-language:higherKinds", // Allow higher-kinded types
  "-language:implicitConversions", // Allow definition of implicit functions called views
  "-language:postfixOps", // Allow postfix operator notation
  "-unchecked"  // Enable additional warnings where generated code depends on assumptions.
)

scalacOptions in (Compile, doc) ++= Seq("-diagrams", "-implicits")

libraryDependencies ++= Seq(
  //"org.scala-lang"         % "scala-reflect"             % scalaVersion.value, //for lambda.scala
  "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.2",
  "org.parboiled"          %% "parboiled"                % "2.1.5",
  "com.lihaoyi"            %% "fastparse"                % "2.1.0",
  "org.scalatest"          %% "scalatest"                % "3.0.7" % "test"
)

//seq(CoverallsPlugin.singleProject: _*)

licenses := Seq("GNU GPL v3" -> url("http://www.gnu.org/licenses/gpl.html"))

homepage := Some(url("http://github.com/newca12/scala-atp"))
