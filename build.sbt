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
  description := "Interpreter of a Sigma-State language",
  pomExtra :=
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
        <developer>
          <id>greenhat</id>
          <name>Denys Zadorozhnyi</name>
          <url>https://github.com/greenhat/</url>
        </developer>
      </developers>,
  publishMavenStyle := true,
  publishTo := sonatypePublishToBundle.value,
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
val debox              = "org.spire-math" %% "debox" % "0.8.0"
val kiama              = "org.bitbucket.inkytonik.kiama" %% "kiama" % "2.1.0"
val fastparse          = "com.lihaoyi" %% "fastparse" % "1.0.0"
val commonsIo          = "commons-io" % "commons-io" % "2.5"
val configs            = "com.github.kxbmap" %% "configs" % "0.4.4"

val specialVersion = "0.6.1"
val meta        = "io.github.scalan" %% "meta" % specialVersion
val plugin      = "io.github.scalan" %% "plugin" % specialVersion
val libraryconf = "io.github.scalan" %% "library-conf" % specialVersion

val testingDependencies = Seq(
  "org.scalatest" %% "scalatest" % "3.0.5" % "test",
  "org.scalactic" %% "scalactic" % "3.0.+" % "test",
  "org.scalacheck" %% "scalacheck" % "1.14.+" % "test",
  "com.storm-enroute" %% "scalameter" % "0.8.2" % Test,
  "junit" % "junit" % "4.12" % "test",
  "com.novocode" % "junit-interface" % "0.11" % "test",
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
  kiama, fastparse, debox
) ++ testingDependencies

val circeVersion = "0.10.0"
val circeCore = "io.circe" %% "circe-core" % circeVersion 
val circeGeneric = "io.circe" %% "circe-generic" % circeVersion 
val circeParser = "io.circe" %% "circe-parser" % circeVersion 

libraryDependencies ++= Seq( circeCore, circeGeneric, circeParser )

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
publishArtifact in Test := true

pomIncludeRepository := { _ => false }

val credentialFile = Path.userHome / ".sbt" / ".sigma-sonatype-credentials"
credentials ++= (for {
  file <- if (credentialFile.exists) Some(credentialFile) else None
} yield Credentials(file)).toSeq

credentials ++= (for {
  username <- Option(System.getenv().get("SONATYPE_USERNAME"))
  password <- Option(System.getenv().get("SONATYPE_PASSWORD"))
} yield Credentials("Sonatype Nexus Repository Manager", "oss.sonatype.org", username, password)).toSeq


// PGP key for signing a release build published to sonatype
// signing is done by sbt-pgp plugin
// how to generate a key - https://central.sonatype.org/pages/working-with-pgp-signatures.html
// how to export a key and use it with Travis - https://docs.scala-lang.org/overviews/contributors/index.html#export-your-pgp-key-pair
pgpPublicRing := file("ci/pubring.asc")
pgpSecretRing := file("ci/secring.asc")
pgpPassphrase := sys.env.get("PGP_PASSPHRASE").map(_.toArray)
usePgpKeyHex("28E27A67AEA38DA458C72228CA9254B5E0640FE4")

def libraryDefSettings = commonSettings ++ testSettings ++ Seq(
  scalacOptions ++= Seq(
//    s"-Xplugin:${file(".").absolutePath }/scalanizer/target/scala-2.12/scalanizer-assembly-core-opt-0d03a785-SNAPSHOT.jar"
  )
)

lazy val common = Project("common", file("common"))
  .settings(commonSettings ++ testSettings,
    libraryDependencies ++= Seq(
      "org.scala-lang" % "scala-reflect" % scalaVersion.value,
      debox, commonsIo
    ))
  .settings(publish / skip := true)

lazy val libraryapi = Project("library-api", file("library-api"))
  .dependsOn(common % allConfigDependency)
  .settings(libraryDefSettings :+ addCompilerPlugin(paradise),
    libraryDependencies ++= Seq(
    ))
  .settings(publish / skip := true)

lazy val libraryimpl = Project("library-impl", file("library-impl"))
  .dependsOn(libraryapi % allConfigDependency)
  .settings(libraryDefSettings,
    libraryDependencies ++= Seq( debox ))
  .settings(publish / skip := true)

lazy val core = Project("core", file("core"))
  .dependsOn(common % allConfigDependency, libraryapi % allConfigDependency)
  .settings(commonSettings,
    libraryDependencies ++= Seq( configs, debox ))
  .settings(publish / skip := true)

lazy val library = Project("library", file("library"))
  .dependsOn(common % allConfigDependency, core % allConfigDependency, libraryapi, libraryimpl)
  .settings(//commonSettings,
    libraryDefSettings ++ testSettings,
    libraryDependencies ++= Seq( debox ))
  .settings(publish / skip := true)

lazy val sigmaconf = Project("sigma-conf", file("sigma-conf"))
  .settings(commonSettings,
    libraryDependencies ++= Seq(
      plugin, libraryconf
    ))
  .settings(publish / skip := true)

lazy val scalanizer = Project("scalanizer", file("scalanizer"))
  .dependsOn(sigmaconf, libraryapi, libraryimpl)
  .settings(commonSettings,
    libraryDependencies ++= Seq(meta, plugin),
    assemblyOption in assembly ~= { _.copy(includeScala = false, includeDependency = true) },
    assemblyMergeStrategy in assembly := {
      case PathList("scalan", xs @ _*) => MergeStrategy.first
      case other => (assemblyMergeStrategy in assembly).value(other)
    },
    artifact in(Compile, assembly) := {
      val art = (artifact in(Compile, assembly)).value
      art.withClassifier(Some("assembly"))
    },
    addArtifact(artifact in(Compile, assembly), assembly)
  )
  .settings(publish / skip := true)

lazy val sigmaapi = Project("sigma-api", file("sigma-api"))
  .dependsOn(common, libraryapi)
  .settings(libraryDefSettings :+ addCompilerPlugin(paradise),
    libraryDependencies ++= Seq(
      macroCompat, scrypto, bouncycastleBcprov
    ))
  .settings(publish / skip := true)

lazy val sigmaimpl = Project("sigma-impl", file("sigma-impl"))
  .dependsOn(
    sigmaapi % allConfigDependency,
    libraryapi % allConfigDependency,
    libraryimpl % allConfigDependency,
    library % allConfigDependency)
  .settings(libraryDefSettings,
    libraryDependencies ++= Seq( scrypto, bouncycastleBcprov ))
  .settings(publish / skip := true)

lazy val sigmalibrary = Project("sigma-library", file("sigma-library"))
  .dependsOn(
    sigmaimpl % allConfigDependency,
    common % allConfigDependency,
    core % allConfigDependency,
    libraryapi % allConfigDependency,
    libraryimpl % allConfigDependency,
    library % allConfigDependency)
  .settings(libraryDefSettings,
    libraryDependencies ++= Seq(
      scrypto,
      bouncycastleBcprov
    ))
  .settings(publish / skip := true)

lazy val sigmastate = (project in file("sigmastate"))
  .dependsOn(sigmaimpl % allConfigDependency, sigmalibrary % allConfigDependency)
  .settings(libraryDefSettings)
  .settings(libraryDependencies ++= Seq(
    scorexUtil, kiama, fastparse, circeCore, circeGeneric, circeParser))
  .settings(publish / skip := true)

lazy val sigma = (project in file("."))
  .aggregate(
    sigmastate, common, core, libraryapi, libraryimpl, library,
    sigmaapi, sigmaimpl, sigmalibrary, sigmaconf, scalanizer)
  .settings(libraryDefSettings, rootSettings)
  .settings(publish / aggregate := false)
  .settings(publishLocal / aggregate := false)

lazy val aggregateCompile = ScopeFilter(
  inProjects(common, core, libraryapi, libraryimpl, library, sigmaapi, sigmaimpl,
    sigmalibrary, sigmastate),
  inConfigurations(Compile))

lazy val rootSettings = Seq(
  sources in Compile := sources.all(aggregateCompile).value.flatten,
  libraryDependencies := libraryDependencies.all(aggregateCompile).value.flatten,
  mappings in (Compile, packageSrc) ++= (mappings in(Compile, packageSrc)).all(aggregateCompile).value.flatten,
  mappings in (Test, packageBin) ++= (mappings in(Test, packageBin)).all(aggregateCompile).value.flatten,
  mappings in(Test, packageSrc) ++= (mappings in(Test, packageSrc)).all(aggregateCompile).value.flatten,
)

def runErgoTask(task: String, sigmastateVersion: String, log: Logger): Unit = {
  val ergoBranch = "sigma-core-opt"
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

def runSpamTestTask(task: String, sigmastateVersion: String, log: Logger): Unit = {
  val spamBranch = "master"
  val envVars = Seq("SIGMASTATE_VERSION" -> sigmastateVersion,
    "SPECIAL_VERSION" -> specialVersion,
    // SSH_SPAM_REPO_KEY should be set (see Jenkins Credentials Binding Plugin)
    "GIT_SSH_COMMAND" -> "ssh -i $SSH_SPAM_REPO_KEY")

  log.info(s"Testing current build with spam tests (branch $spamBranch):")
  val cwd = new File("")
  val spamPath = new File(cwd.absolutePath + "/spam-tests/")
  log.info(s"Cleaning $spamPath")
  s"rm -rf ${spamPath.absolutePath}" !

  log.info(s"Cloning spam tests branch $spamBranch into ${spamPath.absolutePath}")
  Process(Seq("git", "clone",  "-b", spamBranch, "--single-branch", "git@github.com:greenhat/sigma-spam.git", spamPath.absolutePath),
    cwd.getAbsoluteFile,
    envVars: _*) !

  log.info(s"Running spam tests in $spamPath with Sigmastate version $sigmastateVersion")
  val res = Process(Seq("sbt", task), spamPath, envVars: _*) !

  if (res != 0) sys.error(s"Spam $task failed!")
}

lazy val spamTestTask = TaskKey[Unit]("spamTestTask", "run spam tests with current version")
spamTestTask := {
  val log = streams.value.log
  val sigmastateVersion = version.value
  runSpamTestTask("test", sigmastateVersion, log)
}

commands += Command.command("spamTest") { state =>
  "clean" ::
    "publishLocal" ::
    "spamTestTask" ::
    state
}
