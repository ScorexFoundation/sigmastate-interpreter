import scala.language.postfixOps
import scala.sys.process._

organization := "org.scorexfoundation"

name := "sigma-state"

lazy val scala213 = "2.13.8"
lazy val scala212 = "2.12.15"
lazy val scala211 = "2.11.12"

lazy val allConfigDependency = "compile->compile;test->test"

lazy val commonSettings = Seq(
  organization := "org.scorexfoundation",
  crossScalaVersions := Seq(scala213, scala212, scala211),
  scalaVersion := scala212,
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
  scmInfo := Some(
      ScmInfo(
          url("https://github.com/ScorexFoundation/sigmastate-interpreter"),
          "scm:git@github.com:ScorexFoundation/sigmastate-interpreter.git"
      )
  ),
)

// prefix version with "-SNAPSHOT" for builds without a git tag
dynverSonatypeSnapshots in ThisBuild := true
// use "-" instead of default "+"
dynverSeparator in ThisBuild := "-"

val bouncycastleBcprov = "org.bouncycastle" % "bcprov-jdk15on" % "1.64"
val scrypto            = "org.scorexfoundation" %% "scrypto" % "2.1.10"
val scorexUtil         = "org.scorexfoundation" %% "scorex-util" % "0.1.8"
val debox              = "org.scorexfoundation" %% "debox" % "0.9.0"
val spireMacros        = "org.typelevel" %% "spire-macros" % "0.17.0-M1"
val kiama              = "org.bitbucket.inkytonik.kiama" %% "kiama" % "2.5.0"
val fastparse          = "com.lihaoyi" %% "fastparse" % "2.3.3"
val commonsMath3       = "org.apache.commons" % "commons-math3" % "3.2"
val scalaCompat        = "org.scala-lang.modules" %% "scala-collection-compat" % "2.7.0"

val testingDependencies = Seq(
  "org.scalatest" %% "scalatest" % "3.0.9" % Test,
  "org.scalactic" %% "scalactic" % "3.0.9" % Test,
  "org.scalacheck" %% "scalacheck" % "1.14.3" % Test,
  "com.lihaoyi" %% "pprint" % "0.6.3" % Test,
  "com.storm-enroute" %% "scalameter" % "0.19" % Test,
  "junit" % "junit" % "4.12" % Test,
  "com.novocode" % "junit-interface" % "0.11" % Test
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
  kiama, fastparse, debox, spireMacros, scalaCompat
) ++ testingDependencies

lazy val circeCore211 = "io.circe" %% "circe-core" % "0.10.0"
lazy val circeGeneric211 = "io.circe" %% "circe-generic" % "0.10.0"
lazy val circeParser211 = "io.circe" %% "circe-parser" % "0.10.0"

lazy val circeCore = "io.circe" %% "circe-core" % "0.13.0"
lazy val circeGeneric = "io.circe" %% "circe-generic" % "0.13.0"
lazy val circeParser = "io.circe" %% "circe-parser" % "0.13.0"

libraryDependencies ++= Seq(
  if (scalaVersion.value == scala211) circeCore211 else circeCore,
  if (scalaVersion.value == scala211) circeGeneric211 else circeGeneric,
  if (scalaVersion.value == scala211) circeParser211 else circeParser
  )

scalacOptions ++= Seq("-feature", "-deprecation")

// set bytecode version to 8 to fix NoSuchMethodError for various ByteBuffer methods
// see https://github.com/eclipse/jetty.project/issues/3244
// these options applied only in "compile" task since scalac crashes on scaladoc compilation with "-release 8"
// see https://github.com/scala/community-builds/issues/796#issuecomment-423395500
//javacOptions in(Compile, compile) ++= Seq("-target", "8", "-source", "8" )
//scalacOptions in(Compile, compile) ++= Seq("-release", "8")

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
usePgpKeyHex("C1FD62B4D44BDF702CDF2B726FF59DA944B150DD")

def libraryDefSettings = commonSettings ++ testSettings 

lazy val common = Project("common", file("common"))
  .settings(commonSettings ++ testSettings,
    libraryDependencies ++= Seq(
      "org.scala-lang" % "scala-reflect" % scalaVersion.value,
      debox, scalaCompat
    ))
  .settings(publish / skip := true)

lazy val libraryapi = Project("library-api", file("library-api"))
  .dependsOn(common % allConfigDependency)
  .settings(libraryDefSettings, 
    libraryDependencies ++= Seq())
  .settings(publish / skip := true)

lazy val libraryimpl = Project("library-impl", file("library-impl"))
  .dependsOn(libraryapi % allConfigDependency)
  .settings(libraryDefSettings,
    libraryDependencies ++= Seq( debox ))
  .settings(publish / skip := true)

lazy val core = Project("core", file("core"))
  .dependsOn(common % allConfigDependency, libraryapi % allConfigDependency)
  .settings(commonSettings,
    libraryDependencies ++= Seq( debox ))
  .settings(publish / skip := true)

lazy val library = Project("library", file("library"))
  .dependsOn(common % allConfigDependency, core % allConfigDependency, libraryapi, libraryimpl)
  .settings(//commonSettings,
    libraryDefSettings ++ testSettings,
    libraryDependencies ++= Seq( debox ))
  .settings(publish / skip := true)

lazy val sigmaapi = Project("sigma-api", file("sigma-api"))
  .dependsOn(common, libraryapi)
  .settings(libraryDefSettings,
    libraryDependencies ++= Seq(
      scrypto, bouncycastleBcprov
    ))
  .settings(publish / skip := true)

lazy val sigmalibrary = Project("sigma-library", file("sigma-library"))
  .dependsOn(
    common % allConfigDependency,
    core % allConfigDependency,
    libraryapi % allConfigDependency,
    libraryimpl % allConfigDependency,
    library % allConfigDependency,
    sigmaapi % allConfigDependency)
  .settings(libraryDefSettings,
    libraryDependencies ++= Seq(
      scrypto,
      bouncycastleBcprov
    ))
  .settings(publish / skip := true)

lazy val sigmastate = (project in file("sigmastate"))
  .dependsOn(sigmalibrary % allConfigDependency)
  .settings(libraryDefSettings)
  .settings(libraryDependencies ++= Seq(
    scorexUtil, kiama, fastparse, commonsMath3,
    if (scalaVersion.value == scala211) circeCore211 else circeCore,
    if (scalaVersion.value == scala211) circeGeneric211 else circeGeneric,
    if (scalaVersion.value == scala211) circeParser211 else circeParser
    ))
  .settings(publish / skip := true)

lazy val sigma = (project in file("."))
  .aggregate(
    sigmastate, common, core, libraryapi, libraryimpl, library,
    sigmaapi, sigmalibrary)
  .settings(libraryDefSettings, rootSettings)
  .settings(publish / aggregate := false)
  .settings(publishLocal / aggregate := false)

lazy val aggregateCompile = ScopeFilter(
  inProjects(common, core, libraryapi, libraryimpl, library, sigmaapi,
    sigmalibrary, sigmastate),
  inConfigurations(Compile))

lazy val rootSettings = Seq(
  sources in Compile := sources.all(aggregateCompile).value.flatten,
  sourceDirectories in Compile := sourceDirectories.all(aggregateCompile).value.flatten,
  libraryDependencies := libraryDependencies.all(aggregateCompile).value.flatten,
  mappings in (Compile, packageSrc) ++= (mappings in(Compile, packageSrc)).all(aggregateCompile).value.flatten,
  mappings in (Test, packageBin) ++= (mappings in(Test, packageBin)).all(aggregateCompile).value.flatten,
  mappings in(Test, packageSrc) ++= (mappings in(Test, packageSrc)).all(aggregateCompile).value.flatten
)

def runErgoTask(task: String, sigmastateVersion: String, log: Logger): Unit = {
  val ergoBranch = "test-coverage"
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
