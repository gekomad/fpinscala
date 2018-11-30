name := "fpinscala"

version := "0.1"

scalaVersion := "2.12.7"

scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature", "-language:higherKinds")

libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.5" % Test


libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.14.0" % "test"
