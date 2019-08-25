package sigmastate.serialization

import java.math.BigInteger
import java.nio.charset.StandardCharsets

import io.circe._
import io.circe.syntax._
import scalan.RType
import sigmastate._
import sigmastate.eval.{Colls, Evaluation, SigmaDsl}
import sigmastate.lang.SigmaParser
import sigmastate.lang.exceptions.SerializerException
import special.collection.{Coll, collRType}
import special.sigma.BigInt
import spire.syntax.all.cfor

import scala.collection.mutable

object DataJsonEncoder {
  implicit private val encodeBytes: Encoder[Array[Byte]] = (bytes: Array[Byte]) =>
    Json.fromString(bytes.toString)

  private def encodeName[T <: SType](tpe: T): String = tpe match {
    case SUnit => "Unit"
    case SBoolean => "Boolean"
    case SByte => "Byte"
    case SShort => "Short"
    case SInt => "Int"
    case SLong => "Long"
    case SBigInt => "BigInt"
    case SString => "String"
    case tColl: SCollectionType[a] =>
      "Coll[" + encodeName(tColl.elemType) + "]"
    case t: STuple =>
      "(" + t.items.map((x: SType) => s"${encodeName(x)}").mkString(",") + ")"
    case t => throw new SerializerException(s"Not defined DataJsonEncoder for ${t}")
  }

  def encode[T <: SType](v: T#WrappedType, tpe: T): Json = {
    val encodedType = encodeName(tpe)
    val encodedData = encodeData(v, tpe)
    Json.obj(
      "type" -> Json.fromString(encodedType),
      "value" -> encodedData,
    )
  }

  private def encodeData[T <: SType](v: T#WrappedType, tpe: T): Json = tpe match {
    case SUnit => v.asInstanceOf[Unit].asJson
    case SBoolean => v.asInstanceOf[Boolean].asJson
    case SByte => v.asInstanceOf[Byte].asJson
    case SShort => v.asInstanceOf[Short].asJson
    case SInt => v.asInstanceOf[Int].asJson
    case SLong => v.asInstanceOf[Long].asJson
    case SBigInt =>
      java.util.Base64.getEncoder.encodeToString(v.asInstanceOf[BigInt].toBytes.toArray).asJson
    case SString =>
      java.util.Base64.getEncoder.encodeToString(v.asInstanceOf[String].getBytes).asJson
    case tColl: SCollectionType[a] =>
      val coll = v.asInstanceOf[tColl.WrappedType]
      val arr = coll.toArray
      var jsons = mutable.MutableList.empty[Json]
      cfor(0)(_ < arr.length, _ + 1) { i =>
        val x = arr(i)
        jsons += encodeData(x, tColl.elemType)
      }
      Json.fromValues(jsons.toList)
    case t: STuple =>
      val arr = Evaluation.fromDslTuple(v, t).asInstanceOf[t.WrappedType]
      val tArr = t.items.toArray
      val len = arr.length
      assert(len == tArr.length, s"Type $t doesn't correspond to value $arr")
      var obj = mutable.MutableList.empty[(String, Json)]
      cfor(0)(_ < len, _ + 1) { i =>
        obj += (s"_${i + 1}" -> encodeData[SType](arr(i),tArr(i)))
      }
      Json.fromFields(obj.toList)
    case t => throw new SerializerException(s"Not defined DataSerializer for type $t")
  }

  def decodeData[T <: SType](json: Json, tpe: T): (T#WrappedType) = {
    val res = (tpe match {
      case SUnit => Unit
      case SBoolean => json.asBoolean.get
      case SByte => json.asNumber.get.toByte.get
      case SShort => json.asNumber.get.toShort.get
      case SInt => json.asNumber.get.toInt.get
      case SLong => json.asNumber.get.toLong.get
      case SBigInt =>
        val jsonBigInt = json.as[String]
        jsonBigInt match {
          case Right(jsonBigInt) =>
            SigmaDsl.BigInt(new BigInteger(java.util.Base64.getDecoder.decode(jsonBigInt)))
          case Left(error) => throw new SerializerException(error.getMessage)
        }
      case SString =>
        val jsonBigInt = json.as[String]
        jsonBigInt match {
          case Right(jsonBigInt) =>
            new String(java.util.Base64.getDecoder.decode(jsonBigInt))
          case Left(error) => throw new SerializerException(error.getMessage)
        }
      case tColl: SCollectionType[a] =>
        val tpeElem = tColl.elemType
        decodeColl(json, tpeElem)
      case t: STuple =>
        val tArr = t.items.toArray
        val collSource = mutable.ArrayBuilder.make[Any]()
        cfor(1)(_ <= tArr.length, _ + 1) { i =>
          collSource += decodeData(json.hcursor.downField(s"_${i}").focus.get, tArr(i - 1))
        }
        val coll = Colls.fromArray(collSource.result())(RType.AnyType)
        Evaluation.toDslTuple(coll, t)
    }).asInstanceOf[T#WrappedType]
    res
  }

  private def decodeColl[T <: SType](json: Json, tpe: T): Coll[T#WrappedType] = {
    implicit val tItem = (tpe match {
      case tTup: STuple if tTup.items.length == 2 =>
        Evaluation.stypeToRType(tpe)
      case tTup: STuple =>
        collRType(RType.AnyType)
      case _ =>
        Evaluation.stypeToRType(tpe)
    }).asInstanceOf[RType[T#WrappedType]]
    val jsonArray = json.as[List[Json]]
    jsonArray match {
      case Right(jsonArray) =>
        val collSource = mutable.ArrayBuilder.make[T#WrappedType]()(tItem.classTag)
        for (i <- jsonArray) {
          collSource += decodeData(i, tpe)
        }
        Colls.fromArray(collSource.result())
      case Left(error) => throw new SerializerException(error.getMessage)
    }
  }

  def decode(json: Json): (SType#WrappedType)= {
    val cursor: HCursor = json.hcursor
    val tpe = SigmaParser.parseType(json.hcursor.downField("type").focus.get.asString.get)
    val value = json.hcursor.downField("value").focus.get
    val data = decodeData(value, tpe)
    data
  }
}
