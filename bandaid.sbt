import sbt._
import Settings._

lazy val root = project.root
  .setName("bandaid")
  .setDescription("bandaid build")
  .configureRoot
  .aggregate(common)

lazy val common = project.from("ce2")
  .setName("ce2")
  .setDescription("Cats Effect 2 IO bandaid")
  .setInitialImport()
  .configureModule
  .configureTests()

addCommandAlias("fullTest", ";test;scalastyle")
addCommandAlias("fullCoverageTest", ";coverage;test;coverageReport;coverageAggregate;scalastyle")
