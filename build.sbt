name := "verif"

version := "0.0.1"

scalaVersion := "2.12.0"
scalacOptions ++= Seq("-deprecation", "-feature", "-unchecked", "-language:reflectiveCalls")

libraryDependencies += "edu.berkeley.cs" %% "chiseltest" % "0.2.0"

enablePlugins(JmhPlugin)

exportJars := true
resolvers ++= Seq(
  Resolver.sonatypeRepo("snapshots"),
  Resolver.sonatypeRepo("releases"),
  Resolver.mavenLocal
)