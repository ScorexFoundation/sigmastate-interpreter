//package sigmastate.serializer.bytes
//
//import scorex.core.serialization.Serializer
//import sigmastate.serializer.bytes.base._
//import sigmastate.OR
//
//import scala.util.Try
//
//
//class ORSerializer() extends Serializer[OR] {
//  override def toBytes(or: OR): Array[Byte] = {
//    or.input
//  }
//  override def parseBytes(bytes: Array[Byte]): Try[OR] = ???
//}