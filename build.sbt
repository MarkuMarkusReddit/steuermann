name := "steuermann"

version := "0.1"

scalaVersion := "2.13.5"

libraryDependencies += "com.github.tototoshi" %% "scala-csv" % "1.3.7"

assembly / mainClass := Some("Main")
assemblyJarName in assembly := "steuermann.jar"