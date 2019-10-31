package sigmastate.compiler.macros.impl

//import sigmastate.Values
//import sigmastate.Values.SigmaBoolean
//import sigmastate.eval.CSigmaProp

import scala.reflect.macros.whitebox


trait Liftables {
  val c: whitebox.Context

  import c.universe.{Block => _, Constant => _, Function => _, Ident => _, If => _, _}

//  private val pack = q"sigmastate"

//  implicit val cSigmaPropLiftable: Liftable[CSigmaProp] = Liftable[CSigmaProp] { s =>
//    q"sigmastate.eval.CSigmaProp(${s.sigmaTree})"
//  }

//  implicit val cSigmaBooleanLiftable: Liftable[SigmaBoolean] = Liftable[SigmaBoolean] { s => q"$s" }

}
