ThisBuild / scalaVersion     := "2.13.5"
ThisBuild / version          := "0.1.0-SNAPSHOT"
ThisBuild / organization     := "de.funar"
ThisBuild / organizationName := "Active Group GmbH"

lazy val root = (project in file("."))
  .settings(
    name := "Hearts",
    libraryDependencies += "org.scalameta" %% "munit" % "0.7.22" % Test,

    libraryDependencies += "org.typelevel" %% "cats-core" % "2.1.1",
    libraryDependencies += "org.typelevel" %% "cats-effect" % "2.3.1",
    libraryDependencies += "dev.profunktor" %% "console4cats" % "0.8.1",

    libraryDependencies += "org.atnos" %% "eff" % "5.14.0",
    addCompilerPlugin("org.typelevel" %% "kind-projector" % "0.11.3" cross CrossVersion.full),
    libraryDependencies += "org.atnos" %% "eff-cats-effect" % "5.14.0",

    // JSON
    libraryDependencies ++= Seq(
      "io.circe" %% "circe-core",
      "io.circe" %% "circe-generic",
      "io.circe" %% "circe-parser"
    ).map(_ % "0.13.0"),

    libraryDependencies ++= Seq(
      "org.http4s"      %% "http4s-blaze-server",
      "org.http4s"      %% "http4s-blaze-client",
      "org.http4s"      %% "http4s-circe",
      "org.http4s"      %% "http4s-dsl",
    ).map(_ % "1.0.0-M10"),

    testFrameworks += new TestFramework("munit.Framework"),

    scalacOptions ++= Seq(
      "-feature",
      "-deprecation",
      "-unchecked",
      "-language:postfixOps",
      "-language:higherKinds")

    )

