import scala.language.postfixOps
import scala.util.Try
import scala.sys.process._

organization := "org.scorexfoundation"

name := "sigma-state"

lazy val allConfigDependency = "compile->compile;test->test"

lazy val commonSettings = Seq(
  organization := "org.scorexfoundation",
  scalaVersion := "2.12.8",
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
      // see https://docs.travis-ci.com/user/environment-variables/#default-environment-variables
      if (Try(sys.env("TRAVIS")).getOrElse("false") == "true") {
        // pull request number, "false" if not a pull request
        if (Try(sys.env("TRAVIS_PULL_REQUEST")).getOrElse("false") != "false") {
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

val specialVersion = "i8-more-ops-d7c2b8b4-SNAPSHOT"
val specialCommon = "io.github.scalan" %% "common" % specialVersion
val specialCore = "io.github.scalan" %% "core" % specialVersion
val specialLibrary = "io.github.scalan" %% "library" % specialVersion

val specialSigmaVersion = "new-ops-105b47d0-SNAPSHOT"
val sigmaImpl = "io.github.scalan" %% "sigma-impl" % specialSigmaVersion
val sigmaLibrary = "io.github.scalan" %% "sigma-library" % specialSigmaVersion

val testingDependencies = Seq(
  "org.scalatest" %% "scalatest" % "3.0.5" % "test",
  "org.scalactic" %% "scalactic" % "3.0.+" % "test",
  "org.scalacheck" %% "scalacheck" % "1.13.+" % "test",
  "junit" % "junit" % "4.12" % "test",
  "com.novocode" % "junit-interface" % "0.11" % "test",
  specialCommon, (specialCommon % Test).classifier("tests"),
  specialCore, (specialCore % Test).classifier("tests"),
  specialLibrary, (specialLibrary % Test).classifier("tests"),
  sigmaImpl, (sigmaImpl % Test).classifier("tests"),
  sigmaLibrary, (sigmaLibrary % Test).classifier("tests"),
)

libraryDependencies ++= Seq(
  "org.scorexfoundation" %% "scrypto" % "2.1.4",
  "org.scorexfoundation" %% "scorex-util" % "0.1.1",
  "org.bouncycastle" % "bcprov-jdk15on" % "1.+",
  "com.typesafe.akka" %% "akka-actor" % "2.4.+",
  "org.bitbucket.inkytonik.kiama" %% "kiama" % "2.1.0",
  "com.lihaoyi" %% "fastparse" % "1.0.0",
  sigmaImpl,
  sigmaLibrary,
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

def runErgoTask(task: String, sigmastateVersion: String, log: Logger): Unit = {
  val ergoBranch = "v2.0"
  log.info(s"Testing current build in Ergo (branch $ergoBranch):")
  val cwd = new File("").absolutePath
  val ergoPath = new File(cwd + "/ergo-tests/")
  log.info(s"Cleaning $ergoPath")
  s"rm -rf ${ergoPath.absolutePath}" !

  log.info(s"Cloning Ergo branch $ergoBranch into ${ergoPath.absolutePath}")
  s"git clone -b $ergoBranch --single-branch https://github.com/ergoplatform/ergo.git ${ergoPath.absolutePath}" !

  log.info(s"Updating Ergo in $ergoPath with Sigmastate version $sigmastateVersion")
  Process(Seq("sbt", "unlock", "reload", "lock"), ergoPath, "SIGMASTATE_VERSION" -> sigmastateVersion) !

  log.info("Updated Ergo lock.sbt:")
  Process(Seq("git", "diff", "-U0", "lock.sbt"), ergoPath) !

  log.info(s"Running Ergo tests in $ergoPath with Sigmastate version $sigmastateVersion")
  val res = Process(Seq("sbt", task), ergoPath, "SIGMASTATE_VERSION" -> sigmastateVersion) !

  if (res != 0) sys.error(s"Ergo $task failed!")
}

lazy val ergoUnitTest = TaskKey[Unit]("ergoUnitTest", "run ergo unit tests with current version")
ergoUnitTest := {
  val log = streams.value.log
  val sigmastateVersion = version.value
  runErgoTask("test", sigmastateVersion, log) 
}

ergoUnitTest := ergoUnitTest.dependsOn(publishLocal).value

lazy val ergoItTest = TaskKey[Unit]("ergoItTest", "run ergo it:test with current version")
ergoItTest := {
  val log = streams.value.log
  val sigmastateVersion = version.value
  runErgoTask("it:test", sigmastateVersion, log)
}

ergoItTest := ergoItTest.dependsOn(publishLocal).value
