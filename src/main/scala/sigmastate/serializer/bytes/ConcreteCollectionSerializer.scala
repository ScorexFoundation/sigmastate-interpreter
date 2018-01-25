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

class ConcreteCollectionSerializer(implicit d: Serializer[Value[SType]]) extends Serializer[ConcreteCollection[SType]] {

  import ConcreteCollectionSerializer.OpCode

  override def toBytes(c: ConcreteCollection[SType]): Array[Byte] = {
    val typeb = arrayWithKnownSize(TypeBytes.typeCode[SCollection[SType]])
    val r = Bytes.concat(
      shortBytesEnsureCapacity(OpCode),
      arrayWithKnownSize(TypeBytes.typeCode[SCollection[SType]]),
      Bytes.concat(c.value.map(v => d.toBytes(v)).map(arrayWithKnownSize): _*))
    r
  }

  private def parseElems(bytes: Array[Byte]): Try[IndexedSeq[Value[SType]]] = {
    if (bytes.isEmpty) {
      Success(IndexedSeq.empty)
    } else {
    for {
      (elemBytes, rest) <- arrayWithoutKnownSize(bytes)
      elem <- d.parseBytes(elemBytes)
      c <- parseElems(rest).map(s => elem +: s)
    } yield c}
  }

  override def parseBytes(bytes: Array[Byte]): Try[ConcreteCollection[SType]] = {
    println("!")
    for {
      (opCode, bytesAfterOpCode) <- shortBytes(bytes)
      if opCode == OpCode
      (typeBytes, rest) <- arrayWithoutKnownSize(bytesAfterOpCode)
      elems <- parseElems(rest)
    } yield ConcreteCollection[SType](elems)
  }
}
