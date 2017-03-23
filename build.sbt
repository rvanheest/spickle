import sbt.Keys.{ crossScalaVersions, homepage, publishMavenStyle, scmInfo }

lazy val spickleSettings = Seq(
	organization := "com.github.rvanheest",
	name := "spickle",
	version := "1.x-SNAPSHOT",
	scalaVersion := "2.12.0",
	crossScalaVersions := Seq("2.11.8", "2.12.0"),
	licenses += "Apache-2.0" -> url("https://www.apache.org/licenses/LICENSE-2.0.html"),
	homepage := Some(url("https://github.com/rvanheest/spickle")),
	scmInfo := Some(
		ScmInfo(
			browseUrl = url("http://github.com/spickle/spickle"),
			connection = "scm:git:git@github.com:spickle/spickle.git"
		)),
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
	</developers>,
	pomIncludeRepository := { _ => false }
)

lazy val publishSettings = Seq(
	publishTo := {
		val nexus = "https://oss.sonatype.org/"
		if (isSnapshot.value)
			Some("snapshots" at nexus + "content/repositories/snapshots")
		else
			Some("releases" at "service/local/staging/deploy/maven2")
	},
	publishMavenStyle := true
)

lazy val spickleProject = Project(
	id = "spickle",
	base = file("."),
	settings = spickleSettings ++ publishSettings,
	aggregate = Seq(spickleLib)
)

lazy val spickleLib = Project(
	id = "core",
	base = file("core"),
	settings = spickleSettings ++ publishSettings ++ Seq(
		libraryDependencies ++= Seq(
			"org.scala-lang.modules" %% "scala-xml" % "1.0.6",
			"org.scalatest" %% "scalatest" % "3.0.1" % "test"
		)
	)
)
