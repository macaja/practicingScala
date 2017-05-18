name := """practicando"""

version := "1.0"

scalaVersion := "2.11.7"

// Change this to another test framework if you prefer
libraryDependencies ++= Seq(
  "com.typesafe.akka" %% "akka-actor" % "2.3.11",
  "com.typesafe.akka" %% "akka-http" % "10.0.4" ,
  "com.typesafe.akka" %% "akka-http-spray-json" % "10.0.4",
  "com.typesafe.akka" %% "akka-testkit" % "2.3.11" % "test",
  "org.typelevel" %% "cats" % "0.9.0",
  "com.chuusai"                  %% "shapeless"                % "2.3.2",
  "org.scalatest" %% "scalatest" % "2.2.4" % "test")

// Uncomment to use Akka
//libraryDependencies += "com.typesafe.akka" %% "akka-actor" % "2.3.11"

