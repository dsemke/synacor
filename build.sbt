name := "synacor"

version := "0.1"

scalaVersion := "2.12.4"

libraryDependencies += "ch.qos.logback" % "logback-classic" % "1.2.3"
libraryDependencies += "com.typesafe.scala-logging" %% "scala-logging" % "3.7.2"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.4" % "test"

test in assembly := {}
//mainClass in assembly := Some("Main"),