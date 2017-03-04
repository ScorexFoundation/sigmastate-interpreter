name := "sigma"

version := "0.0.1"

scalaVersion := "2.12.1"

val testingDependencies = Seq(
  "org.scalatest" %% "scalatest" % "3.0.1" % "test",
  "org.scalactic" %% "scalactic" % "3.0.1" % "test",
  "org.scalacheck" %% "scalacheck" % "1.13.+" % "test"
)

libraryDependencies ++= Seq(
  "com.typesafe.akka" %% "akka-actor" % "2.4.+",
  "org.scorexfoundation" %% "scorex-core" % "2.+"
) ++ testingDependencies

//scalacOptions in Compile ++= Seq("-Xprompt", "-Ydebug", "-verbose" )



















