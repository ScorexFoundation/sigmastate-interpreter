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
        <developer>
          <id>aslesarenko</id>
          <name>Alexander Slesarenko</name>
          <url>https://github.com/aslesarenko/</url>
        </developer>
      </developers>,
  publishMavenStyle := true,
  publishTo := {
    val nexus = "https://oss.sonatype.org/"
    if (isSnapshot.value) { Some("snapshots" at nexus + "content/repositories/snapshots") }
    else { Some("releases" at nexus + "service/local/staging/deploy/maven2") }
  }

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

val bouncycastleBcprov = "org.bouncycastle" % "bcprov-jdk15on" % "1.60"
val scrypto            = "org.scorexfoundation" %% "scrypto" % "2.1.6"
val scorexUtil         = "org.scorexfoundation" %% "scorex-util" % "0.1.4"
val macroCompat        = "org.typelevel" %% "macro-compat" % "1.1.1"
val paradise           = "org.scalamacros" %% "paradise" % "2.1.0" cross CrossVersion.full

val specialVersion = "master-61e8ec8c-SNAPSHOT"
val specialCommon  = "io.github.scalan" %% "common" % specialVersion
val specialCore    = "io.github.scalan" %% "core" % specialVersion
val specialLibrary = "io.github.scalan" %% "library" % specialVersion

val meta        = "io.github.scalan" %% "meta" % specialVersion
val plugin      = "io.github.scalan" %% "plugin" % specialVersion
val libraryapi  = "io.github.scalan" %% "library-api" % specialVersion
val libraryimpl = "io.github.scalan" %% "library-impl" % specialVersion
val libraryconf = "io.github.scalan" %% "library-conf" % specialVersion

val testingDependencies = Seq(
  "org.scalatest" %% "scalatest" % "3.0.5" % "test",
  "org.scalactic" %% "scalactic" % "3.0.+" % "test",
  "org.scalacheck" %% "scalacheck" % "1.14.+" % "test",
  "junit" % "junit" % "4.12" % "test",
  "com.novocode" % "junit-interface" % "0.11" % "test",
  specialCommon, (specialCommon % Test).classifier("tests"),
  specialCore, (specialCore % Test).classifier("tests"),
  specialLibrary, (specialLibrary % Test).classifier("tests"),
)

lazy val testSettings = Seq(
  libraryDependencies ++= testingDependencies,
  parallelExecution in Test := false,
  baseDirectory in Test := file("."),
  publishArtifact in Test := true,
  publishArtifact in(Test, packageSrc) := true,
  publishArtifact in(Test, packageDoc) := false,
  test in assembly := {})

libraryDependencies ++= Seq(
  scrypto,
  scorexUtil,
  "org.bouncycastle" % "bcprov-jdk15on" % "1.+",
  "com.typesafe.akka" %% "akka-actor" % "2.4.+",
  "org.bitbucket.inkytonik.kiama" %% "kiama" % "2.1.0",
  "com.lihaoyi" %% "fastparse" % "1.0.0",
  "org.spire-math" %% "debox" % "0.8.0"
) ++ testingDependencies


scalacOptions ++= Seq("-feature", "-deprecation")


// set bytecode version to 8 to fix NoSuchMethodError for various ByteBuffer methods
// see https://github.com/eclipse/jetty.project/issues/3244
// these options applied only in "compile" task since scalac crashes on scaladoc compilation with "-release 8"
// see https://github.com/scala/community-builds/issues/796#issuecomment-423395500
javacOptions in(Compile, compile) ++= Seq("-target", "8", "-source", "8" )
scalacOptions in(Compile, compile) ++= Seq("-release", "8")

//uncomment lines below if the Scala compiler hangs to see where it happens
//scalacOptions in Compile ++= Seq("-Xprompt", "-Ydebug", "-verbose" )

parallelExecution in Test := false
publishArtifact in Test := false

pomIncludeRepository := { _ => false }

credentials += Credentials(Path.userHome / ".sbt" / ".sigma-sonatype-credentials")

credentials ++= (for {
  username <- Option(System.getenv().get("SONATYPE_USERNAME"))
  password <- Option(System.getenv().get("SONATYPE_PASSWORD"))
} yield Credentials("Sonatype Nexus Repository Manager", "oss.sonatype.org", username, password)).toSeq

def libraryDefSettings = commonSettings ++ testSettings ++ Seq(
  scalacOptions ++= Seq(
//        s"-Xplugin:${file(".").absolutePath }/scalanizer/target/scala-2.12/scalanizer-assembly-better-costing-2a66ed5c-SNAPSHOT.jar"
  )
)

lazy val sigmaconf = Project("sigma-conf", file("sigma-conf"))
    .settings(commonSettings,
      libraryDependencies ++= Seq(
        plugin, libraryconf
      ))

lazy val scalanizer = Project("scalanizer", file("scalanizer"))
    .dependsOn(sigmaconf)
    .settings(commonSettings,
      libraryDependencies ++= Seq(meta, plugin, libraryapi, libraryimpl),
//      publishArtifact in(Compile, packageBin) := false,
      assemblyOption in assembly ~= { _.copy(includeScala = false, includeDependency = true) },
      artifact in(Compile, assembly) := {
        val art = (artifact in(Compile, assembly)).value
        art.withClassifier(Some("assembly"))
      },
      addArtifact(artifact in(Compile, assembly), assembly)
    )

lazy val sigmaapi = Project("sigma-api", file("sigma-api"))
    .settings(libraryDefSettings :+ addCompilerPlugin(paradise),
      libraryDependencies ++= Seq(
        specialCommon, libraryapi, macroCompat, scrypto, bouncycastleBcprov
      ))

lazy val sigmaimpl = Project("sigma-impl", file("sigma-impl"))
    .dependsOn(sigmaapi % allConfigDependency)
    .settings(libraryDefSettings,
      libraryDependencies ++= Seq(
        libraryapi, libraryimpl, scrypto, bouncycastleBcprov
      ))

lazy val sigmalibrary = Project("sigma-library", file("sigma-library"))
    .dependsOn(sigmaimpl % allConfigDependency)
    .settings(libraryDefSettings,
      libraryDependencies ++= Seq(
        specialCommon, (specialCommon % Test).classifier("tests"),
        specialCore, (specialCore % Test).classifier("tests"),
        libraryapi, (libraryapi % Test).classifier("tests"),
        libraryimpl, (libraryimpl % Test).classifier("tests"),
        specialLibrary, (specialLibrary % Test).classifier("tests"),
        scrypto,
        bouncycastleBcprov
      ))

lazy val sigma = (project in file("."))
    .aggregate(sigmaapi, sigmaimpl, sigmalibrary, sigmaconf, scalanizer)
    .dependsOn(sigmaimpl % allConfigDependency, sigmalibrary % allConfigDependency)
    .settings(commonSettings: _*)

def runErgoTask(task: String, sigmastateVersion: String, log: Logger): Unit = {
  val ergoBranch = "master"
  val sbtEnvVars = Seq("BUILD_ENV" -> "test", "SIGMASTATE_VERSION" -> sigmastateVersion)
  
  log.info(s"Testing current build in Ergo (branch $ergoBranch):")
  val cwd = new File("").absolutePath
  val ergoPath = new File(cwd + "/ergo-tests/")
  log.info(s"Cleaning $ergoPath")
  s"rm -rf ${ergoPath.absolutePath}" !

  log.info(s"Cloning Ergo branch $ergoBranch into ${ergoPath.absolutePath}")
  s"git clone -b $ergoBranch --single-branch https://github.com/ergoplatform/ergo.git ${ergoPath.absolutePath}" !


  log.info(s"Updating Ergo in $ergoPath with Sigmastate version $sigmastateVersion")
  Process(Seq("sbt", "unlock", "reload", "lock"), ergoPath, sbtEnvVars: _*) !

  log.info("Updated Ergo lock.sbt:")
  Process(Seq("git", "diff", "-U0", "lock.sbt"), ergoPath) !

  log.info(s"Running Ergo tests in $ergoPath with Sigmastate version $sigmastateVersion")
  val res = Process(Seq("sbt", task), ergoPath, sbtEnvVars: _*) !

  if (res != 0) sys.error(s"Ergo $task failed!")
}

lazy val ergoUnitTestTask = TaskKey[Unit]("ergoUnitTestTask", "run ergo unit tests with current version")
ergoUnitTestTask := {
  val log = streams.value.log
  val sigmastateVersion = version.value
  runErgoTask("test", sigmastateVersion, log) 
}

commands += Command.command("ergoUnitTest") { state =>
  "clean" ::
    "publishLocal" ::
    "ergoUnitTestTask" ::
    state
}

lazy val ergoItTestTask = TaskKey[Unit]("ergoItTestTask", "run ergo it:test with current version")
ergoItTestTask := {
  val log = streams.value.log
  val sigmastateVersion = version.value
  runErgoTask("it:test", sigmastateVersion, log)
}

commands += Command.command("ergoItTest") { state =>
  "clean" ::
    "publishLocal" ::
    "ergoItTestTask" ::
    state
}
