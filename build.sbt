name := "sigma"

version := "1.0"

scalaVersion := "2.11.8"

val testingDependencies = Seq(
  "org.scalatest" %% "scalatest" % "2.+" % "test",
  "org.scalactic" %% "scalactic" % "2.+" % "test",
  "org.scalacheck" %% "scalacheck" % "1.12.+" % "test"
)

libraryDependencies ++= Seq(
  "com.typesafe.akka" %% "akka-actor" % "2.4.+",
  "org.scorexfoundation" %% "scorex-core" % "2.+"
) ++ testingDependencies





















