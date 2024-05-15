package scalan

trait TestLibrary extends Scalan {
  import CollBuilder._
  import SigmaDslBuilder._
  val reflection = (GraphIRReflection)
  lazy val colBuilder: Ref[CollBuilder] = variable[CollBuilder]
  lazy val sigmaDslBuilder: Ref[SigmaDslBuilder] = variable[SigmaDslBuilder]
}
