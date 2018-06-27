package sigmastate.lang

import org.ergoplatform.{Height, Outputs, Inputs}
import scalan.SigmaLibrary
import sigmastate._
import sigmastate.Values.{Value, Constant}
import sigmastate.serialization.OpCodes
import sigmastate.utxo.SizeOf

class SigmaCoster[Ctx <: SigmaLibrary](val ctx: Ctx) {
  import SigmaCoster._
  import ctx._

  def stypeToElem[T <: SType](t: T): Elem[T#WrappedType] = (t match {
    case SByte => ByteElement
    case SShort => ShortElement
    case SInt => IntElement
    case SLong => LongElement
    case _ => error(s"Don't know how to convert SType $t to Elem")
  }).asElem[T#WrappedType]

  private val elemToNumericMap = Map[Elem[_], Numeric[_]](
    (ByteElement, numeric[Byte]),
    (ShortElement, numeric[Short]),
    (IntElement, numeric[Int]),
    (LongElement, numeric[Long])
  )
  def elemToNumeric[T](e: Elem[T]): Numeric[T] = elemToNumericMap(e).asInstanceOf[Numeric[T]]

  def opcodeToBinOp[T](opCode: Byte, eT: Elem[T]): BinOp[T,T] = opCode match {
    case OpCodes.PlusCode => NumericPlus(elemToNumeric(eT))(eT)
    case _ => error(s"Cannot find BinOp for opcode $opCode")
  }

  private def evalNode[T <: SType](ctx: Rep[Context], node: Value[T]): Rep[Costed[T#WrappedType]] = {
    val res: Rep[Any] = node match {
      case Constant(v, tpe) => toRep(v)(stypeToElem(tpe))
      case Height => ctx.HEIGHT
      case Inputs => ctx.INPUTS
      case Outputs => ctx.OUTPUTS
      case SizeOf(xs) =>
        val xs1 = evalNode(ctx, xs).asRep[Col[Any]]
        xs1.length
      case op: ArithOp[t] =>
        val tpe = op.left.tpe
        val et = stypeToElem(tpe)
        val binop = opcodeToBinOp(op.opCode, et)
        val x = evalNode(ctx, op.left)
        val y = evalNode(ctx, op.right)
        ApplyBinOp(binop, x, y)
      case _ =>
        error(s"Don't know how to evalNode($node)")
    }
    res.asRep[Costed[T#WrappedType]]
  }

  def buildCostedGraph[T <: SType](tree: Value[T]): Rep[Context => Costed[T#WrappedType]] = {
    fun { ctx: Rep[Context] => evalNode(ctx, tree) }
  }
}

object SigmaCoster {
  def error(msg: String) = throw new CosterException(msg, None)
}
