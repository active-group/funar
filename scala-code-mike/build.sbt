ThisBuild / scalaVersion     := "2.13.5"
ThisBuild / version          := "0.1.0-SNAPSHOT"
ThisBuild / organization     := "de.funar"
ThisBuild / organizationName := "Active Group GmbH"

lazy val root = (project in file("."))
  .settings(
    name := "FUNAR",
    libraryDependencies += "org.scalameta" %% "munit" % "0.7.22" % Test,
    libraryDependencies += "org.scalameta" %% "munit-scalacheck" % "0.7.22" % Test,

    libraryDependencies += "org.typelevel" %% "cats-core" % "2.1.1",
    libraryDependencies += "org.typelevel" %% "cats-effect" % "2.3.1",
    libraryDependencies += "dev.profunktor" %% "console4cats" % "0.8.1",

    addCompilerPlugin("org.typelevel" %% "kind-projector" % "0.11.3" cross CrossVersion.full),

    testFrameworks += new TestFramework("munit.Framework"),

    scalacOptions ++= Seq(
      "-feature",
      "-deprecation",
      "-unchecked",
      "-language:postfixOps",
      "-language:higherKinds")

    )

