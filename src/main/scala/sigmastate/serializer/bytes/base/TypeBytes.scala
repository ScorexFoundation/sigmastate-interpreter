package sigmastate.serializer.bytes.base

import sigmastate.{SType, _}

import scala.language.higherKinds
import scala.reflect.runtime.universe._

object TypeBytes {
  def typeCode[T <: SType](implicit t: WeakTypeTag[T]): Array[Byte] = {
    def aux(tpe: Type): Array[Byte] = {
      tpe match {
        case TypeRef(_, col, tps) if col == typeOf[SCollection[_]].typeSymbol => 9.toByte +: aux(tps.head)
        case t if t == weakTypeOf[SInt.type].resultType => Array(1.toByte)
        case t if t == weakTypeOf[SBigInt.type].resultType => Array(2.toByte)
        case t if t == weakTypeOf[SBoolean.type].resultType => Array(3.toByte)
        case t if t == weakTypeOf[SByteArray.type].resultType => Array(4.toByte)
        case t if t == weakTypeOf[SProp.type].resultType => Array(5.toByte)
        case t if t == weakTypeOf[SAvlTree.type].resultType => Array(6.toByte)
        case t if t == weakTypeOf[SGroupElement.type].resultType => Array(7.toByte)
        case t if t == weakTypeOf[SBox.type].resultType => Array(8.toByte)
      }
    }
    aux(t.tpe)
  }

  def checkCollectionType[T <: SType](code: Array[Byte], c: Seq[Value[T]])(implicit t: TypeTag[T]): Boolean = {
    typeCode[T].sameElements(code)
  }
}
