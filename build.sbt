import scala.util.Try

organization := "org.scorexfoundation"

name := "sigma-state"

lazy val commonSettings = Seq(
  organization := "org.scorexfoundation",
  scalaVersion := "2.12.4",
  resolvers += Resolver.sonatypeRepo("public"),
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

enablePlugins(GitVersioning)

version in ThisBuild := {
  if (git.gitCurrentTags.value.nonEmpty) {
    git.gitDescribedVersion.value.get
  } else {
    if (git.gitHeadCommit.value.contains(git.gitCurrentBranch.value)) {
      if (Try(sys.env("TRAVIS")).getOrElse("false") == "true") {
        if (Try(sys.env("TRAVIS_PULL_REQUEST")).getOrElse("false") == "true") {
          // build is triggered by a pull request
          val prBranchName = Try(sys.env("TRAVIS_PULL_REQUEST_BRANCH")).get
          val prHeadCommitSha = Try(sys.env("TRAVIS_PULL_REQUEST_SHA")).get
          prBranchName + "-" + prHeadCommitSha.take(8) + "-SNAPSHOT"
        } else {
          // build is triggered by a push
          val branchName = Try(sys.env("TRAVIS_BRANCH")).get
          branchName + "-" + git.gitHeadCommit.value.get.take(8) + "-SNAPSHOT"
        }
      } else {
        git.gitHeadCommit.value.get.take(8) + "-SNAPSHOT"
      }
    } else {
      git.gitCurrentBranch.value + "-" + git.gitHeadCommit.value.get.take(8) + "-SNAPSHOT"
    }
  }
}

git.gitUncommittedChanges in ThisBuild := true

val testingDependencies = Seq(
  "org.scalatest" %% "scalatest" % "3.0.+" % "test",
  "org.scalactic" %% "scalactic" % "3.0.+" % "test",
  "org.scalacheck" %% "scalacheck" % "1.13.+" % "test",
  "junit" % "junit" % "4.12" % "test",
  "com.novocode" % "junit-interface" % "0.11" % "test"
)

libraryDependencies ++= Seq(
  "org.scorexfoundation" %% "scrypto" % "2.1.3",
  "org.scorexfoundation" %% "scorex-util" % "0.1.1",
  "org.bouncycastle" % "bcprov-jdk15on" % "1.+",
  "com.typesafe.akka" %% "akka-actor" % "2.4.+",
  "org.bitbucket.inkytonik.kiama" %% "kiama" % "2.1.0",
  "com.lihaoyi" %% "fastparse" % "1.0.0"
) ++ testingDependencies


scalacOptions ++= Seq("-feature", "-deprecation")

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

credentials += Credentials(Path.userHome / ".sbt" / ".sigma-sonatype-credentials")

credentials ++= (for {
  username <- Option(System.getenv().get("SONATYPE_USERNAME"))
  password <- Option(System.getenv().get("SONATYPE_PASSWORD"))
} yield Credentials("Sonatype Nexus Repository Manager", "oss.sonatype.org", username, password)).toSeq

lazy val sigma = (project in file(".")).settings(commonSettings: _*)
