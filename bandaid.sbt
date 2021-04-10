import sbt._
import Settings._

Global / excludeLintKeys ++= Set(scalacOptions, trapExit)

lazy val root = project.root
  .setName("bandaid")
  .setDescription("bandaid build")
  .configureRoot
  .aggregate(ce2)

lazy val ce2 = project.from("ce2")
  .setName("ce2")
  .setDescription("Cats Effect 2 IO bandaid")
  .setInitialImport()
  .configureModule
  .configureTests()
  .settings(libraryDependencies += Dependencies.ce2)
  .settings(libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value)

// TODO: prepare similar utility for ce3

addCommandAlias("fullTest", ";test;scalastyle")
addCommandAlias("fullCoverageTest", ";coverage;test;coverageReport;coverageAggregate;scalastyle")
