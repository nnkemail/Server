name := """Server"""

version := "1.0-SNAPSHOT"

EclipseKeys.preTasks := Seq(compile in Compile)

lazy val root = (project in file(".")).enablePlugins(PlayScala)

scalaVersion := "2.11.6"

libraryDependencies ++= Seq(
  ws,
  specs2 % Test,
  "com.typesafe.akka" %% "akka-agent"   % "2.4.0",
  "com.typesafe.akka" %% "akka-slf4j" % "2.4.0",
  "com.typesafe.akka" %% "akka-remote" % "2.4.1",
  cache
)

resolvers += "scalaz-bintray" at "http://dl.bintray.com/scalaz/releases"
resolvers += "Atlassian Releases" at "https://maven.atlassian.com/public/"
resolvers += "sonatype-releases" at "https://oss.sonatype.org/content/repositories/releases/"

// Play provides two styles of routers, one expects its actions to be injected, the
// other, legacy style, accesses its actions statically.
routesGenerator := InjectedRoutesGenerator

fork in run := false
