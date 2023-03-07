logLevel := Level.Warn

addSbtPlugin("com.eed3si9n" % "sbt-assembly" % "0.14.9")
addSbtPlugin("com.eed3si9n" % "sbt-buildinfo" % "0.9.0")
addSbtPlugin("net.virtual-void" % "sbt-dependency-graph" % "0.9.2")
addSbtPlugin("org.scoverage" % "sbt-scoverage" % "1.6.1")
addSbtPlugin("org.scoverage" % "sbt-coveralls" % "1.2.7")
addSbtPlugin("org.xerial.sbt" % "sbt-sonatype" % "3.9.17")
addSbtPlugin("com.github.sbt" % "sbt-pgp" % "2.2.1")
addSbtPlugin("com.dwijnand" % "sbt-dynver" % "4.1.1")