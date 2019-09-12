name := "fpinscala"

version := "0.2"

scalaVersion := "2.13.0"

scalacOptions ++= Seq(
  "-deprecation",
  "-encoding", "UTF-8",
  "-language:higherKinds",
  "-language:postfixOps",
  "-feature"
)

libraryDependencies += "org.scalatest" %% "scalatest" % "3.1.0-RC2" % Test

libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.14.0" % Test
