name := "swiss-commercial-register"

version := "1.0"

scalaVersion := "2.13.10"

libraryDependencies ++= Seq("com.typesafe.akka" %% "akka-actor" % "2.7.0",
                            "org.scalaj" %% "scalaj-http" % "2.4.2")

ThisBuild / scalafmtOnCompile := true
