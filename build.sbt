name := "scala-atp"

organization := "org.edla"

version := "0.1-SNAPSHOT"

scalaVersion := "2.10.2"

scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature", "-optimize")

scalacOptions in (Compile, doc) ++= Seq("-diagrams","-implicits")

org.scalastyle.sbt.ScalastylePlugin.Settings

//resolvers += "Local Maven Repository" at "file://"+Path.userHome.absolutePath+"/.m2/repository"
resolvers += "edla repo" at "http://www.edla.org/snapshots"

libraryDependencies ++= Seq(
  "at.logic" % "fol" % "1.0-SNAPSHOT",
  "at.logic" % "ivy" % "1.0-SNAPSHOT",
  "org.parboiled" %% "parboiled-scala" % "1.1.5",
  "junit" % "junit" % "4.11" % "test",
  "org.specs2" %% "specs2" % "1.14" % "test"
)

// Uncomment the following line to use one-jar (https://github.com/sbt/sbt-onejar)
//seq(com.github.retronym.SbtOneJar.oneJarSettings: _*)

licenses := Seq("GNU GPL v3" -> url("http://www.gnu.org/licenses/gpl.html"))

homepage := Some(url("http://github.com/newca12/scala-atp"))

//pomIncludeRepository := { _ => false }

pomExtra := (
  <!-- pluginRepository needed for add-source goal
  -->
  <pluginRepositories>
	<pluginRepository>
		<id>el4.elca-services.ch</id>
        <name>el4</name>
        <url>http://el4.elca-services.ch/el4j/maven2repository</url>
    </pluginRepository>
  </pluginRepositories>
  <scm>
    <url>git@github.com:newca12/ambassy.git</url>
    <connection>scm:git:git@github.com:newca12/ambassy.git</connection>
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
		<!-- source and test directories not handled yet by sbt make-pom so added manually -->
		<sourceDirectory>src/main/scala</sourceDirectory>
		<testSourceDirectory>src/test/scala</testSourceDirectory>
		<plugins>
			<plugin>
				<groupId>org.apache.maven.plugins</groupId>
				<artifactId>maven-compiler-plugin</artifactId>
				<version>3.1</version>
				<configuration>
					<source>1.7</source>
					<target>1.7</target>
				</configuration>
			</plugin>		
			<plugin>
				<groupId>net.alchim31.maven</groupId>
				<artifactId>scala-maven-plugin</artifactId>
				<version>3.1.5</version>
				<executions>
					<execution>
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
				<version>2.12.4</version>
				<configuration>
					<useFile>false</useFile>
					<disableXmlReport>true</disableXmlReport>
					<!-- If you have classpath issue like NoDefClassError,... -->
					<!-- useManifestOnlyJar>false</useManifestOnlyJar -->
					<includes>
						<include>**/*Spec.*</include>
					</includes>
					<excludes>
						<exclude>**/*.off</exclude>
					</excludes>
				</configuration>
			</plugin>
		</plugins>
	</build>
	<reporting>
		<plugins>
			<plugin>
				<groupId>net.alchim31.maven</groupId>
				<artifactId>scala-maven-plugin</artifactId>
				<version>3.1.5</version>
			</plugin>
		</plugins>
	</reporting>
)
