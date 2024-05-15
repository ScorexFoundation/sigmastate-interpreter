package scalan

import sigma.compiler.{GraphIRReflection, Scalan}

trait TestLibrary extends Scalan {
  import CollBuilder._
  import SigmaDslBuilder._
  val reflection = (GraphIRReflection)
}
