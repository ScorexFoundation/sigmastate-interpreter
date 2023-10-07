package sigmastate.eval

import debox.cfor
import sigma.Extensions.ArrayOps
import sigma._
import sigma.data._

import scala.annotation.unused
import scala.reflect.ClassTag


/** This class represents context variable and register value of a functional type A => B.
  * When variable or register is accessed using `getVar[A => B](id).get` or
  * `box.getReg[A => B].get an instance of this class is returned.
  *
  * It internally transforms a given `tree` into executable function.
  * This it similar to what happens during validation of propositions in the input boxes:
  * - size check of underlying ErgoTree against limits
  * - construction of `calcF` and `costF` graphs, both are stored together with resulting function.
  * - check the types of `calcF` graph to be compatible with expected types A and B
  * If anything goes wrong, this operation fails and if it is used in the script, the script also fails.
  *
  * When f is obtained as `val f = getVar[Int => Int](id).get` then any application `f(x)` involves size estimation
  * using underlying `costF(x)`.
  * */
//case class CFunc[A,B](context: sigmastate.interpreter.Context, tree: SValue)
//    (implicit tDom: RType[A], tRange: RType[B], IR: IRContext) extends (A => B) {
//  import CFunc._
//
//  private val compiled = {
//    import IR._
//    val IR.Pair(calcF, costF) = IR.doCosting(emptyEnv, tree)
//
//    val eDom = asElem[Any](IR.rtypeToElem(tDom))
//    val eRange = asElem[Any](IR.rtypeToElem(tRange))
//
//    IR.verifyCalcFunc[Any => Any](asRep[Context => (Any => Any)](calcF), IR.funcElement(eDom, eRange))
////    IR.verifyCostFunc(costF).getOrThrow
////    IR.verifyIsProven(calcF).getOrThrow
//
//    // check cost
////    val costingCtx = context.toSigmaContext(IR, isCost = true)
////    val costFun = IR.compile[SInt.type](IR.getDataEnv, costF)
////    val IntConstant(estimatedCost) = costFun(costingCtx)
////    if (estimatedCost > maxCost) {
////      throw new Error(s"Estimated execution cost $estimatedCost exceeds the limit $maxCost in $tree")
////    }
//    // check calc
//    val calcCtx = context.toSigmaContext(IR, isCost = false)
//    val valueFun = IR.compile[SFunc](IR.getDataEnv, asRep[Context => SFunc#WrappedType](calcF))
//    val res = valueFun(calcCtx) match {
//      case Constant(f, fTpe: SFunc) => f
//      case v => v
//    }
//    res.asInstanceOf[A => B]
//  }
//
//  override def apply(x: A): B = compiled(x)
//}
object CFunc {
  /** The cost of creating resulting function but not its execution.
    * Thus it is expected to be small. It can be increased if useful cases are found
    * such that `tree` should contains heavy operations. */
  val maxCost = 1000
}

/** A default implementation of [[Context]] interface.
  * @see [[Context]] for detailed descriptions
  */
case class CContext(
    _dataInputs: Coll[Box],
    override val headers: Coll[Header],
    override val preHeader: PreHeader,
    inputs: Coll[Box],
    outputs: Coll[Box],
    height: Int,
    selfBox: Box,
    private val selfIndex: Int,
    lastBlockUtxoRootHash: AvlTree,
    _minerPubKey: Coll[Byte],
    vars: Coll[AnyValue],
    override val activatedScriptVersion: Byte,
    override val currentErgoTreeVersion: Byte
) extends Context {
  @inline override def builder: SigmaDslBuilder = CSigmaDslBuilder

  @inline override def HEIGHT: Int = height

  @inline override def SELF: Box = selfBox

  @inline override def dataInputs: Coll[Box] = _dataInputs

  @inline override def INPUTS = inputs

  @inline override def OUTPUTS = outputs

  @inline override def LastBlockUtxoRootHash = lastBlockUtxoRootHash

  @inline override def minerPubKey = _minerPubKey

  override def selfBoxIndex: Int = {
    if (VersionContext.current.isJitActivated) {
      // starting from v5.0 this is fixed
      selfIndex
    } else {
      // this used to be a bug in v4.x (https://github.com/ScorexFoundation/sigmastate-interpreter/issues/603)
      -1
    }
  }

  override def getVar[T](id: Byte)(implicit tT: RType[T]): Option[T] = {
    @unused // avoid warning about unused ctA
    implicit val tag: ClassTag[T] = tT.classTag
    if (id < 0 || id >= vars.length) return None
    val value = vars(id)
    if (value != null) {
      // once the value is not null it should be of the right type
      value match {
        case value: CAnyValue[_] if value.value != null && value.tA == tT =>
          Some(value.value.asInstanceOf[T])
        case _ =>
          throw new InvalidType(s"Cannot getVar[${tT.name}]($id): invalid type of value $value at id=$id")
      }
    } else None
  }

  /** Return a new context instance with variables collection updated.
    * @param bindings  a new binding of the context variables with new values.
    * @return a new instance (if `bindings` non-empty) with the specified bindings.
    *         other existing bindings are copied to the new instance
    */
  def withUpdatedVars(bindings: (Int, AnyValue)*): CContext = {
    if (bindings.isEmpty) return this

    val ids = bindings.map(_._1).toArray
    val values = bindings.map(_._2).toArray
    val maxVarId = ids.max  // INV: ids is not empty
    val requiredNewLength = maxVarId + 1

    val newVars = if (vars.length < requiredNewLength) {
      // grow vars collection
      val currVars = vars.toArray
      val buf = new Array[AnyValue](requiredNewLength)
      Array.copy(currVars, 0, buf, 0, currVars.length)
      cfor(0)(_ < ids.length, _ + 1) { i =>
        buf(ids(i)) = values(i)
      }
      buf.toColl
    } else {
      vars.updateMany(ids.toColl, values.toColl)
    }

    this.copy(vars = newVars)
  }
}

