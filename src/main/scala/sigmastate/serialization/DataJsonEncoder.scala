package sigmastate.serialization

import java.nio.charset.StandardCharsets

import io.circe._
import io.circe.syntax._
import sigmastate._
import sigmastate.eval.SigmaDsl
import sigmastate.lang.exceptions.SerializerException
import special.collection.Coll
import special.sigma.BigInt
import spire.syntax.all.cfor

import scala.collection.mutable
import scala.collection.mutable.{MutableList, Stack}

object STypeNameEncoder {
  def encode[T <: SType](tpe: T): String = tpe match {
    case SUnit => "Unit"
    case SBoolean => "Boolean"
    case SByte => "Byte"
    case SShort => "Short"
    case SInt => "Int"
    case SLong => "Long"
    case SBigInt => "BigInt"
    case SString => "String"
    case tColl: SCollectionType[a] =>
      "Coll[" + encode(tColl.elemType) + "]"
    case t => throw new SerializerException(s"Not defined DataJsonEncoder for ${t}")
  }

  sealed trait Chunk

  case class NameChunk(data: String)
  case class ParenthesesChunk(chunks: List[Chunk])
  case class BracketsChunk(typename: NameChunk, wrappedTypeChunk: Chunk)

  private class ChunkParser(from: String) {
    var position = 0

    private def checkPosition(pos: Int): Unit = {
      if (pos >= from.length) {
        throw new SerializerException("Out of bounds character search.")
      }
    }
    private def getBracketChunk(typename: NameChunk): BracketsChunk = {
      val wrappedChunks = getChunks
      val newPosition = position + 1
      checkPosition(newPosition)
      if (from(newPosition) != ']') {
        throw new SerializerException("Bad brackets balance: closing `]` is not found at right level")
      }
      position += 1
      BracketsChunk(typename, wrappedChunks)
    }

    private def getParenthesesChunk: ParenthesesChunk = {
      var chunks = mutable.MutableList.empty[Chunk]
      chunks += getChunks
      position += 1
      checkPosition(position)
      while (from(position) == ',') {
        chunks += getChunks
        val newPosition = position + 1
        checkPosition(newPosition)
      }
      if (from(position) != ')') {
        throw new SerializerException("Bad brackets balance: closing `)` is not found at right level")
      }
      ParenthesesChunk(chunks.toList)
    }

    private def getNameChunk: NameChunk = {
      var name: String = ""
      while (from(position).isLetter && 'A'.t)
    }
    def getChunks: Chunk = {
      checkPosition(position)

      from(position) match {
        case  =>
          position += 1
          getBracketChunk()
      }
    }
  }
  private class Decoder(from: String) {
    private var stack = mutable.MutableList.empty[Chunk]
    var position = 0

    private def isUsualLetter(c: Char): Boolean = c.isLetter && c >= 'A' && c <= 'z'

    private def readTypeName(): String = {
      var result = ""
      while (isUsualLetter(from(position))) {
        result += from(position)
      }
      result
    }

    private def chunkParse: Chunk = {
      if (position >= from.length) {
        throw new SerializerException(s"Can't shrink to chunks ${from}.")
      }
      from(position) match {
        case '(' =>
          var parenthesisLevel = 0
          var currentPosition = position + 1
          while (currentPosition < from.length) {
            match from(curr)
            currentPosition += 1
          }
          stack += new Chunk(ChunkType.OpeningBracket)

      }

      return SUnit
    }
    def get: SType = {

    }
  }
  def decode(from: String): SType = {

  }



}
object DataJsonEncoder {
  implicit private val encodeBytes: Encoder[Array[Byte]] = (bytes: Array[Byte]) => Json.obj(
    "length" -> Json.fromInt(bytes.length),
    "data" -> Json.fromString(bytes.toString)
  )

  private def encodeSTypeData[T <: SType](v: T#WrappedType, tpe: T): Json = tpe match {
    case SUnit => v.asInstanceOf[Unit].asJson
    case SBoolean => v.asInstanceOf[Boolean].asJson
    case SByte => v.asInstanceOf[Byte].asJson
    case SShort => v.asInstanceOf[Short].asJson
    case SInt => v.asInstanceOf[Int].asJson
    case SLong => v.asInstanceOf[Long].asJson
    case SBigInt =>
      val bytes = SigmaDsl.toBigInteger(v.asInstanceOf[BigInt]).toByteArray
      bytes.asJson
    case SString =>
      val bytes = v.asInstanceOf[String].getBytes(StandardCharsets.UTF_8)
      bytes.asJson
    case tColl: SCollectionType[a] =>
      val coll = v.asInstanceOf[tColl.WrappedType]
      tColl.elemType match {
        case SBoolean =>
          coll.asInstanceOf[Coll[Boolean]].toArray.asJson
        case SByte =>
          coll.asInstanceOf[Coll[Byte]].toArray.asJson
        case _ =>
          val arr = coll.toArray
          val jsons = mutable.MutableList.empty[Json]
          cfor(0)(_ < arr.length - 1, _ + 1) { i =>
            val x = arr(i)
            jsons += encodeSTypeData(x, tColl.elemType)
          }
          jsons.asJson
      }
  }
//
//  def encode[T <: SType](v: T#WrappedType, tpe: T): Json = {
//    val encodedType = encodeSTypeName(tpe)
//    val encodedData = encodeSTypeData(v, tpe)
//    Json.obj(
//      "type" -> Json.fromString(encodedType),
//      "value" -> encodedData,
//    )
//  }
//
//  private def deserialize[T <: SType](tpe: T, json: Json): (T#WrappedType) = (tpe match {
//    case SUnit => ()
//    case SBoolean => r.getUByte() != 0
//    case SByte => r.getByte()
//    case SShort => r.getShort()
//    case SInt => r.getInt()
//    case SLong => r.getLong()
//    case SString =>
//      val size = r.getUInt().toInt
//      val bytes = r.getBytes(size)
//      new String(bytes, StandardCharsets.UTF_8)
//    case SBigInt =>
//      val size: Short = r.getUShort().toShort
//      if (size > SBigInt.MaxSizeInBytes)
//        throw new SerializerException(s"BigInt value doesn't not fit into ${SBigInt.MaxSizeInBytes} bytes: $size")
//      val valueBytes = r.getBytes(size)
//      SigmaDsl.BigInt(new BigInteger(valueBytes))
//    case SGroupElement =>
//      SigmaDsl.GroupElement(GroupElementSerializer.parse(r))
//    case SSigmaProp =>
//      SigmaDsl.SigmaProp(SigmaBoolean.serializer.parse(r))
//    case SBox =>
//      SigmaDsl.Box(ErgoBox.sigmaSerializer.parse(r))
//    case SAvlTree =>
//      SigmaDsl.avlTree(AvlTreeData.serializer.parse(r))
//    case tColl: SCollectionType[a] =>
//      val len = r.getUShort()
//      if (tColl.elemType == SByte)
//        Colls.fromArray(r.getBytes(len))
//      else
//        deserializeColl(len, tColl.elemType, r)
//    case tuple: STuple =>
//      val arr = tuple.items.map { t =>
//        deserialize(t, r)
//      }.toArray[Any]
//      val coll = Colls.fromArray(arr)(RType.AnyType)
//      Evaluation.toDslTuple(coll, tuple)
//    case t =>
//      CheckSerializableTypeCode(t.typeCode)
//      throw new SerializerException(s"Not defined DataSerializer for type $t")
//  }).asInstanceOf[T#WrappedType]
}
