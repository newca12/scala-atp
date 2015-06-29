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

resolvers += "edla repo" at "http://www.edla.org/snapshots"

libraryDependencies ++= Seq(
  "at.logic.gapt" %% "gapt" % "1.10-SNAPSHOT",
  "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.4",
  //workaround for ivy : impossible to get artifacts when data has not been loaded.
  //http://harrah.github.io/xsbt/latest/sxr/Ivy.scala.html
  //"org.scala-lang.modules" %% "scala-xml" % "1.0.4",
  "org.parboiled" %% "parboiled" % "2.2.0-SNAPSHOT",
  "com.lihaoyi" %% "fastparse" % "0.2.1",
  "org.scalatest" %% "scalatest" % "2.2.5" % "test",
  "com.twitter" % "util-core_2.11" % "6.24.0"
)

//seq(CoverallsPlugin.singleProject: _*)

// Uncomment the following line to use one-jar (https://github.com/sbt/sbt-onejar)
//seq(com.github.retronym.SbtOneJar.oneJarSettings: _*)

licenses := Seq("GNU GPL v3" -> url("http://www.gnu.org/licenses/gpl.html"))

homepage := Some(url("http://github.com/newca12/scala-atp"))

//pomIncludeRepository := { _ => false }

pomExtra := (
  <scm>
    <url>git@github.com:newca12/scala-atp.git</url>
    <connection>scm:git:git@github.com:newca12/scala-atp.git</connection>
  </scm>
  <developers>
    <developer>
      <id>newca12</id>
      <name>Olivier ROLAND</name>
      <url>http://www.edla.org</url>
    </developer>
  </developers>
  <contributors>
  </contributors>
	<properties>
		<encoding>UTF-8</encoding>
		<project.build.sourceEncoding>UTF-8</project.build.sourceEncoding>
	</properties>
	<build>
		<sourceDirectory>src/main/scala</sourceDirectory>
		<testSourceDirectory>src/test/scala</testSourceDirectory>
		<plugins>
			<plugin>
				<groupId>org.apache.maven.plugins</groupId>
				<artifactId>maven-compiler-plugin</artifactId>
				<version>3.2</version>
				<configuration>
					<source>1.8</source>
					<target>1.8</target>
				</configuration>
			</plugin>
			<plugin>
				<groupId>net.alchim31.maven</groupId>
				<artifactId>scala-maven-plugin</artifactId>
				<version>3.2.0</version>
				<executions>
					<execution>
						<id>compile</id>
						<goals>
							<goal>compile</goal>
							<goal>testCompile</goal>
						</goals>
					</execution>
				</executions>
			</plugin>
			<plugin>
				<groupId>org.apache.maven.plugins</groupId>
				<artifactId>maven-surefire-plugin</artifactId>
				<version>2.18.1</version>
				<configuration>
					<includes>
						<include>**/*Suite.class</include>
						<include>**/*Test.class</include>
						<include>**/*Tests.class</include>
						<include>**/*Spec.class</include>
						<include>**/*Specs.class</include>
					</includes>
				</configuration>
			</plugin>
		</plugins>
	</build>
	<reporting>
		<plugins>
			<plugin>
				<groupId>net.alchim31.maven</groupId>
				<artifactId>scala-maven-plugin</artifactId>
				<version>3.2.0</version>
			</plugin>
		</plugins>
	</reporting>
)
