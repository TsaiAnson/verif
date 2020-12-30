name := "verif"
organization := "edu.berkeley.cs"
version := "0.0.1-SNAPSHOT"
scalaVersion := "2.12.12"

val directoryLayout = Seq(
  scalaSource in Compile := baseDirectory.value / "src",
  javaSource in Compile := baseDirectory.value / "src",
  resourceDirectory in Compile := baseDirectory.value / "src" / "resources",
  scalaSource in Test := baseDirectory.value / "test",
  resourceDirectory in Test := baseDirectory.value / "test" / "resources",
)

val buildSettings = Seq(
  resolvers ++= Seq(
    Resolver.sonatypeRepo("snapshots"),
    Resolver.sonatypeRepo("releases"),
    Resolver.mavenLocal
  ),
  scalacOptions := Seq("-deprecation", "-unchecked", "-Xsource:2.11", "-language:reflectiveCalls"),
  libraryDependencies += "edu.berkeley.cs" %% "chiseltest" % "0.3.1",
  libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.+" % "test"
)

lazy val core = (project in file("./core"))
  .settings(buildSettings)
  .settings(directoryLayout)

lazy val tilelink = (project in file("./tilelink"))
  .dependsOn(core)
  .settings(buildSettings)
  .settings(directoryLayout)

fork in test := true
cancelable in Global := true
exportJars := true
