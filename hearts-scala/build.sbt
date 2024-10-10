ThisBuild / scalaVersion     := "3.2.2"
ThisBuild / version          := "0.1.0-SNAPSHOT"
ThisBuild / organization     := "de.funar"
ThisBuild / organizationName := "Active Group GmbH"

lazy val root = (project in file("."))
  .settings(
    name := "Hearts",
    libraryDependencies += "org.scalameta" %% "munit" % "0.7.29" % Test,

    libraryDependencies += "org.typelevel" %% "cats-core" % "2.9.0",
    libraryDependencies += "org.typelevel" %% "cats-effect" % "3.5.0",
    // libraryDependencies += "dev.profunktor" %% "console4cats" % "0.8.1",

    libraryDependencies += "org.atnos" %% "eff" % "6.0.3",
    libraryDependencies += "org.atnos" %% "eff-cats-effect" % "6.0.3",

    // JSON
    libraryDependencies ++= Seq(
      "io.circe" %% "circe-core",
      "io.circe" %% "circe-generic",
      "io.circe" %% "circe-parser"
    ).map(_ % "0.14.5"),

    libraryDependencies ++= Seq(
      "org.http4s"      %% "http4s-blaze-server",
      "org.http4s"      %% "http4s-blaze-client",
      "org.http4s"      %% "http4s-circe",
      "org.http4s"      %% "http4s-dsl",
    ).map(_ % "1.0.0-M36"),

    testFrameworks += new TestFramework("munit.Framework"),

    scalacOptions ++= Seq(
      "-feature",
      "-deprecation",
      "-unchecked",
      "-language:postfixOps",
      "-language:higherKinds",
      "-language:implicitConversions",
      "-Ykind-projector:underscores")
    )

