package sigmastate.serialization

import sigmastate.SType
import sigmastate.Values.{Constant, Value}
import sigmastate.eval.IRContext

class ErgoTreeSerializer(IR: IRContext) {
  import IR._

  def serialize(tree: Value[SType]): Array[Byte] = ???
  def deserialize(bytes: Array[Byte]): Value[SType] = ???

  def extractConstants(tree: Value[SType]): (Seq[Constant[_]], Value[SType]) = {
    val env = Map[String, Any]()
    val Pair(calcF, _) = doCosting(env, tree)
    val outTree = IR.buildTree(calcF)
    (Seq(), outTree)
  }
}

object ErgoTreeSerializer {
  def apply(IR: IRContext): ErgoTreeSerializer = new ErgoTreeSerializer(IR)
}
