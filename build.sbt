scalaVersion := "2.11.2"

organization := "org.lancegatlin"

name := "scala_typeclass_macro"

version := "0.1-SNAPSHOT"

scalacOptions ++= Seq("-feature","-unchecked", "-deprecation")
  
libraryDependencies += "org.scalatest" % "scalatest_2.11" % "2.2.0" % "test"

libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value