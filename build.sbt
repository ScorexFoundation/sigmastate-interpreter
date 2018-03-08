name := "sigma-state"

version := "0.0.1"

scalaVersion := "2.12.4"

val testingDependencies = Seq(
  "org.scalatest" %% "scalatest" % "3.0.+" % "test",
  "org.scalactic" %% "scalactic" % "3.0.+" % "test",
  "org.scalacheck" %% "scalacheck" % "1.13.+" % "test",
  "junit" % "junit" % "4.12" % "test",
  "com.novocode" % "junit-interface" % "0.11" % "test"
)

libraryDependencies ++= Seq(
  "org.scorexfoundation" %% "scrypto" % "2.+",
  //"org.bouncycastle" % "bcprov-jdk15on" % "1.50"
  "com.typesafe.akka" %% "akka-actor" % "2.4.+",
  "org.bitbucket.inkytonik.kiama" %% "kiama" % "2.1.+",
  "com.lihaoyi" %% "fastparse" % "1.0.0"
) ++ testingDependencies


//uncomment lines below if the Scala compiler hangs to see where it happens
//scalacOptions in Compile ++= Seq("-Xprompt", "-Ydebug", "-verbose" )