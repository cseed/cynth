name := "cynth"

version := "1.0"

scalaVersion := "2.12.1"

enablePlugins(JavaAppPackaging)

libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.5"

libraryDependencies += "org.scalatest" % "scalatest_2.12" % "3.0.1" % "test"

mainClass in Compile := Some("cs.cynth.Main")
