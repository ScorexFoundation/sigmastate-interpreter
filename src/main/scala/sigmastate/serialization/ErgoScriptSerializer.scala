package sigmastate.serialization

import sigmastate.SType
import sigmastate.Values.{Constant, Value}
import sigmastate.eval.RuntimeIRContext

class ErgoScriptSerializer(IR: RuntimeIRContext) {
  import IR._

  def serialize(script: Value[SType]): Array[Byte] = ???
  def deserialize(bytes: Array[Byte]): Value[SType] = ???

  def extractConstants(script: Value[SType]): (Seq[Constant[_]], Value[SType]) = {
    val env = Map[String, Any]()
    val IR.Pair(calcF, _) = doCosting(env, script)
    val tree = IR.buildTree(calcF)
    (Seq(), tree)
  }
}

object ErgoScriptSerializer {
  def apply(IR: RuntimeIRContext): ErgoScriptSerializer = new ErgoScriptSerializer(IR)
}
