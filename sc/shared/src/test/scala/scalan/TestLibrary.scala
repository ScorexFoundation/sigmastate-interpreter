package scalan

trait TestLibrary extends Scalan {
  import CollBuilder._
  val reflection = (GraphIRReflection)
  lazy val colBuilder: Ref[CollBuilder] = variable[CollBuilder]
}
