package scalan

trait TestLibrary extends Library {
  import CollBuilder._

  lazy val colBuilder: Ref[CollBuilder] = variable[CollBuilder]
}
