package scalan

trait TestLibrary extends Library {
  import CollBuilder._
  val reflection = (GraphIRReflection)
  lazy val colBuilder: Ref[CollBuilder] = variable[CollBuilder]
}
