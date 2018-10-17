package sigmastate.serialization

import sigmastate.SType
import sigmastate.Values.{Constant, Value}
import sigmastate.eval.RuntimeIRContext

object ErgoScriptSerializer {
  def serialize(script: Value[SType]): Array[Byte] = ???
  def deserialize(bytes: Array[Byte]): Value[SType] = ???

  def extractConstants(script: Value[SType]): (Seq[Constant[_]], Value[SType]) = {
    val IR: RuntimeIRContext = new RuntimeIRContext {}
    import IR._
    implicit val eAny = stypeToElem(script.tpe).asInstanceOf[Elem[Context]]

    val env = Map[String, Any]()
    val IR.Pair(calcF, _) = doCosting(env, script)

    val f = fun { in: Rep[Context] =>
      val graph = calcF(in)
      // todo traverse the graph and swap Const with ConstantPlaceholder
      graph
    }

    val tree = IR.buildTree(f.asRep[Context => SType#WrappedType])
    (Seq(), tree)
  }
}
