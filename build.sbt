ThisBuild / scalaVersion := "2.13.6"
ThisBuild / version := "0.1.0-SNAPSHOT"
ThisBuild / organization := "io.github.sitiritis"
ThisBuild / organizationName := "sitiritis"

lazy val root = (project in file("."))
  .settings(Compiler.settings)
  .settings(
    name := "data-types-a-la-carte",
    libraryDependencies ++= Dependencies.dependencies
  )
