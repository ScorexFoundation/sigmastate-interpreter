package sigmastate.lang

import scalan.{SigmaLibrary}
import sigmastate._
import sigmastate.Values.{Value, Constant}
import sigmastate.serialization.OpCodes

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

  private def evalNode[T <: SType](node: Value[T]): Rep[T#WrappedType] = node match {
    case Constant(v, tpe) => toRep(v)(stypeToElem(tpe))
    case op: ArithOp[t] =>
      val tpe = op.left.tpe
      val et = stypeToElem(tpe)
      val binop = opcodeToBinOp(op.opCode, et)
      val x = evalNode(op.left)
      val y = evalNode(op.right)
      ApplyBinOp(binop, x, y)
    case _ =>
      error(s"Don't know how to evalNode($node)")
  }

  def buildCostedGraph[T <: SType](tree: Value[T]): Rep[T#WrappedType] = {
    evalNode(tree)
  }
}

object SigmaCoster {
  def error(msg: String) = throw new CosterException(msg, None)
}
