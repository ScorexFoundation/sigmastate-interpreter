organization := "org.scorexfoundation"

name := "sigma-state"

lazy val allConfigDependency = "compile->compile;test->test"

lazy val commonSettings = Seq(
  organization := "org.scorexfoundation",
  version := "0.9.4",
  scalaVersion := "2.12.4",
  licenses := Seq("CC0" -> url("https://creativecommons.org/publicdomain/zero/1.0/legalcode")),
  homepage := Some(url("https://github.com/ScorexFoundation/sigmastate-interpreter")),
  pomExtra :=
    <scm>
      <url>git@github.com:ScorexProject/scrypto.git</url>
      <connection>git@github.com:ScorexFoundation/sigmastate-interpreter.git</connection>
    </scm>
      <developers>
        <developer>
          <id>kushti</id>
          <name>Alexander Chepurnoy</name>
          <url>http://chepurnoy.org/</url>
        </developer>
      </developers>
)

val testingDependencies = Seq(
  "org.scalatest" %% "scalatest" % "3.0.+" % "test",
  "org.scalactic" %% "scalactic" % "3.0.+" % "test",
  "org.scalacheck" %% "scalacheck" % "1.13.+" % "test",
  "junit" % "junit" % "4.12" % "test",
  "com.novocode" % "junit-interface" % "0.11" % "test",
  "special" %% "library" % "0.3.0-SNAPSHOT" % "test",
  "special.sigma" %% "sigma-library" % "0.3.0-SNAPSHOT" % "test"
)

libraryDependencies ++= Seq(
  "org.scorexfoundation" %% "scrypto" % "2.1.+",
  "org.bouncycastle" % "bcprov-jdk15on" % "1.+",
  "com.typesafe.akka" %% "akka-actor" % "2.4.+",
  "org.bitbucket.inkytonik.kiama" %% "kiama" % "2.1.0",
  "com.lihaoyi" %% "fastparse" % "1.0.0",
  "special" %% "library" % "0.3.0-SNAPSHOT",
  "special.sigma" %% "sigma-library" % "0.3.0-SNAPSHOT"
) ++ testingDependencies


//uncomment lines below if the Scala compiler hangs to see where it happens
//scalacOptions in Compile ++= Seq("-Xprompt", "-Ydebug", "-verbose" )


publishMavenStyle := true

parallelExecution in Test := false
publishArtifact in Test := false

publishTo := {
  val nexus = "https://oss.sonatype.org/"
  if (isSnapshot.value) { Some("snapshots" at nexus + "content/repositories/snapshots") }
  else { Some("releases"  at nexus + "service/local/staging/deploy/maven2") }
}

pomIncludeRepository := { _ => false }

lazy val sigma = (project in file(".")).settings(commonSettings: _*)
