scalaVersion := "2.13.5"
organization := "dev.gsoto"
name := "pukeko"

scalacOptions ++= Seq(
  "-feature",
  "-unchecked",
  "-deprecation",
  "-encoding", "utf-8",
  "-explaintypes",
  "-Xcheckinit",
  "-Xfatal-warnings",
  "-Xlint",
  "-Wdead-code",
  "-Wnumeric-widen",
  "-Wunused",
  "-Wvalue-discard",
)
