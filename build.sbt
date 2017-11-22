name := "swiss-commercial-register"

version := "1.0"

scalaVersion := "2.11.12"

libraryDependencies ++= Seq("com.typesafe.akka" % "akka-actor_2.11" % "2.4.20",
                            "org.scalaj" %% "scalaj-http" % "2.2.2")

scalafmtOnCompile in ThisBuild := true
