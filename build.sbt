organization := "com.github.rvanheest"

name := "spickle"

version := "1.x-SNAPSHOT"

scalaVersion := "2.12.0"

crossScalaVersions := Seq("2.10.6", "2.11.8", "2.12.0")

libraryDependencies += "org.scala-lang.modules" % "scala-xml_2.12" % "1.0.6"

licenses += "Apache-2.0" -> url("https://www.apache.org/licenses/LICENSE-2.0.html")

homepage := Some(url("https://github.com/rvanheest/spickle"))

scmInfo := Some(
	ScmInfo(
		browseUrl = url("http://github.com/spickle/spickle"),
		connection = "scm:git:git@github.com:spickle/spickle.git"
	)
)

pomExtra := <developers>
	<developer>
		<id>rvanheest</id>
		<name>Richard van Heest</name>
		<url>https://github.com/rvanheest</url>
		<roles>
			<role>Developer</role>
		</roles>
		<timezone>+1</timezone>
	</developer>
</developers>

pomIncludeRepository := { x => false }

publishTo := {
	val nexus = "https://oss.sonatype.org/"
	if (isSnapshot.value)
		Some("snapshots" at nexus + "content/repositories/snapshots")
	else
		Some("releases" at "service/local/staging/deploy/maven2")
}

publishMavenStyle := true
