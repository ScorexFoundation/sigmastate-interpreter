package sigmastate

import sigmastate.interpreter.Interpreter

trait TestsBase {

  val ActivatedVersionInTest = Interpreter.MaxSupportedScriptVersion

}
