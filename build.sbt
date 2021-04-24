ThisBuild / scalaVersion := "2.13.5"
ThisBuild / organization := "dev.gsoto"

lazy val hello = (project in file("."))
  .settings(
    name := "pukeko"
  )
