package sigmastate.serializer.bytes

import com.google.common.primitives.Bytes
import scorex.core.serialization.Serializer
import sigmastate._
import sigmastate.serializer.bytes.BytesDeserializer._
import sigmastate.serializer.bytes.BytesSerializer._
import sigmastate.serializer.bytes.base.TypeBytes

import scala.reflect.runtime.universe._
import scala.util.{Success, Try}

object ConcreteCollectionSerializer {
  val OpCode: Short = 38
}

class ConcreteCollectionSerializer[ElemType <: SType : TypeTag](implicit d: Serializer[Value[ElemType]]) extends Serializer[ConcreteCollection[ElemType]] {

  import ConcreteCollectionSerializer.OpCode

  override def toBytes(c: ConcreteCollection[ElemType]): Array[Byte] = {
    val typeb = arrayWithKnownSize(TypeBytes.typeCode[SCollection[ElemType]])
    val r = Bytes.concat(
      shortBytesEnsureCapacity(OpCode),
      arrayWithKnownSize(TypeBytes.typeCode[SCollection[ElemType]]),
      Bytes.concat(c.value.map(v => d.toBytes(v)).map(arrayWithKnownSize): _*))
    r
  }

  private def parseElems(bytes: Array[Byte]): Try[IndexedSeq[Value[ElemType]]] = {
    if (bytes.isEmpty) {
      Success(IndexedSeq.empty)
    } else {
    for {
      (elemBytes, rest) <- arrayWithoutKnownSize(bytes)
      elem <- d.parseBytes(elemBytes)
      c <- parseElems(rest).map(s => elem +: s)
    } yield c}
  }

  override def parseBytes(bytes: Array[Byte]): Try[ConcreteCollection[ElemType]] = {
    println("!")
    for {
      (opCode, bytesAfterOpCode) <- shortBytes(bytes)
      if opCode == OpCode
      (typeBytes, rest) <- arrayWithoutKnownSize(bytesAfterOpCode)
      elems <- parseElems(rest)
    } yield ConcreteCollection(elems)
  }
}
