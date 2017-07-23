import sbt.Keys.{ crossScalaVersions, homepage, publishMavenStyle, scmInfo }

lazy val spickleSettings = Seq(
  organization := "com.github.rvanheest",
  version := "1.x-SNAPSHOT",
  scalaVersion := "2.12.0",
  crossScalaVersions := Seq("2.11.11", "2.12.2"),
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
  publishMavenStyle := true,
  publishArtifact in Test := false
)

lazy val root = project.in(file("."))
  .settings(spickleSettings,
    name := "root",
    publishArtifact := false
  )
  .aggregate(spickleLib)

lazy val spickleLib = Project(
  id = "spickle",
  base = file("core"),
  settings = spickleSettings ++ publishSettings ++ Seq(
    name := "spickle",
    libraryDependencies ++= Seq(
      "org.scala-lang.modules" %% "scala-xml" % "1.0.6" % "provided",
      "org.scalatest" %% "scalatest" % "3.0.1" % "test",
      "org.scoverage" %% "scalac-scoverage-runtime" % "1.3.0"
    )
  )
)

lazy val spickleExample = Project(
  id = "spickle-example",
  base = file("example"),
  settings = spickleSettings ++ Seq(
    name := "spickle-example",
    libraryDependencies ++= Seq(
      "org.scala-lang.modules" %% "scala-xml" % "1.0.6"
    ),
    publishArtifact := false
  )
).dependsOn(spickleLib)
