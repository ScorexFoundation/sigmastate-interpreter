logLevel := Level.Warn

addSbtPlugin("com.eed3si9n" % "sbt-assembly" % "0.14.9")
addSbtPlugin("com.eed3si9n" % "sbt-buildinfo" % "0.9.0")
addSbtPlugin("com.typesafe.sbt" % "sbt-git" % "1.0.0")
addSbtPlugin("net.virtual-void" % "sbt-dependency-graph" % "0.9.2")
addSbtPlugin("org.scoverage" % "sbt-scoverage" % "1.6.1")
addSbtPlugin("org.scoverage" % "sbt-coveralls" % "1.2.7")
addSbtPlugin("org.xerial.sbt" % "sbt-sonatype" % "3.8")
addSbtPlugin("com.jsuereth" % "sbt-pgp" % "2.0.0")

resolvers ++= Seq(
  Resolver.bintrayRepo("epfl-lara", "princess"),
  Resolver.bintrayIvyRepo("epfl-lara", "sbt-plugins"),
  "uuverifiers" at "http://logicrunch.research.it.uu.se/maven",
)

addSbtPlugin("ch.epfl.lara" % "sbt-stainless" % "0.3.2")