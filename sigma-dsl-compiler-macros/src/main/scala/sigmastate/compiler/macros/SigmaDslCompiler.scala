package sigmastate.compiler.macros

import sigmastate.compiler.macros.impl.SigmaDslCompilerImp

import scala.language.experimental.macros

object SigmaDslCompiler {

  def compile(contract: Any => Boolean): Unit = macro SigmaDslCompilerImp.compile


//  def serializedErgoTree(contract: Context => Boolean): Array[Byte] = ???

}
