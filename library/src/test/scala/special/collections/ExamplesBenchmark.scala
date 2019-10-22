package special.collections

import scalan.util.CollectionUtil.outerJoin
import org.scalameter.api.{Bench, Gen}
import org.scalameter.picklers.noPickler._
import scalan.util.CollectionUtil._

trait ExamplesBenchmarkCases extends CollGens with ExampleGens { suite: Bench[Double] =>
  import Examples._
  val examples = new Examples(builder)
  import examples._
  val context = genContext.sample.get
  val contextG = Gen.single("context")(context)

  type ContextData = (Array[Array[(Array[Byte], Long)]], Array[Array[(Array[Byte], Long)]])
  def getContextData(ctx: Context): ContextData = {
    val ins = ctx.inputs.map(box => box.tokens.map(t => (t._1.toArray, t._2)).toArray).toArray
    val outs = ctx.outputs.map(box => box.tokens.map(t => (t._1.toArray, t._2)).toArray).toArray
    (ins, outs)
  }

  def checkTokenBalanceWithData(ctx: ContextData): Boolean = {
    val input = ctx._1.flatMap(ts => ts).toIterable.mapReduce[Array[Byte], Long](t => t)((a,b) => a + b).toMap
    val output = ctx._2.flatMap(ts => ts).toIterable.mapReduce[Array[Byte], Long](t => t)((a,b) => a + b).toMap
    val flagged = outerJoin(input, output)(
    (onlyIn: Array[Byte], _) => false,
    (onlyOut: Array[Byte], _) => false,
    { case (tokenId, inV, outV) => inV == outV })
    flagged.forall { case (tokenId, ok) => ok }
  }

  val contextData = getContextData(context)
  val contextDataG = Gen.single("contextData")(contextData)

  require(checkTokenBalanceWithData(contextData) == checkTokenBalance(context))

  performance of "Examples" in {
    measure method "checkTokenBalanceWithData" in {
      using(contextDataG) in {
        ctx => checkTokenBalanceWithData(ctx)
      }
    }
    measure method "checkTokenBalance" in {
      using(contextG) in {
        ctx => checkTokenBalance(ctx)
      }
    }
  }
}

object FastExamplesBenchmark extends Bench.LocalTime with ExamplesBenchmarkCases {
}

