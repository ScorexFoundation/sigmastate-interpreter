package scalan

import sigma.compiler.{GraphIRReflection, IRContext}

trait TestLibrary extends IRContext {
  import CollBuilder._
  import SigmaDslBuilder._
  val reflection = (GraphIRReflection)
}
