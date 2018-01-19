package sigmastate.serializer.bytes

import com.google.common.primitives.Bytes
import scorex.core.serialization.Serializer
import sigmastate._
import sigmastate.serializer.bytes.BytesDeserializer._
import sigmastate.serializer.bytes.BytesSerializer._
import sigmastate.serializer.bytes.base.TypeBytes

import scala.reflect.runtime.universe._
import scala.util.Try

object ConcreteCollectionSerializer {
  val OpCode: Short = 38
}

abstract class ConcreteCollectionSerializer[ElemType <: SType : TypeTag](implicit d: Serializer[Value[ElemType]]) extends Serializer[ConcreteCollection[ElemType]] {

  import ConcreteCollectionSerializer.OpCode

  override def toBytes(c: ConcreteCollection[ElemType]): Array[Byte] = {
    Bytes.concat(
      shortBytesEnsureCapacity(OpCode),
      arrayWithKnownSize(TypeBytes.typeCode[ElemType]),
      Bytes.concat(c.value.map(_.bytes).map(arrayWithKnownSize): _*))
  }

  private def parseElems(bytes: Array[Byte]): Try[IndexedSeq[Value[ElemType]]] = {
    for {
      (elemBytes, rest) <- arrayWithoutKnownSize(bytes)
      elem <- d.parseBytes(elemBytes)
      c <- parseElems(rest).map(s => elem +: s)
    } yield c
  }

  override def parseBytes(bytes: Array[Byte]): Try[ConcreteCollection[ElemType]] = {
    for {
      (opCode, bytesAfterOpCode) <- shortBytes(bytes)
      if opCode == OpCode
      elems <- parseElems(bytesAfterOpCode)
    } yield ConcreteCollection(elems)
  }
}
