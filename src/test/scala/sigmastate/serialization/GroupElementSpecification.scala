package sigmastate.serialization

import java.math.BigInteger

import org.bouncycastle.math.ec.{ECFieldElement, ECPoint}
import org.bouncycastle.math.ec.custom.djb.{Curve25519, Curve25519Point}
import sigmastate.Values.BooleanConstant

import scala.util.Try

class GroupElementSpecification extends TableSerializationSpecification {

  ignore("GroupElementSpecification serialization") {

/*    val (msgArray: Array[Byte], point: Curve25519Point) = CreatePoint

    val array = scapi.sigma.Curve25519.decodeGroupElementToByteArray(point)

    var element = scapi.sigma.Curve25519.encodeByteArrayToGroupElement(array)

    val curv = new Curve25519()

    val point1 = curv.createPoint(
      BigInteger.valueOf(1),
      BigInteger.valueOf(2))
      .asInstanceOf[Curve25519Point]

    val array2 = scapi.sigma.Curve25519.decodeGroupElementToByteArray(point1)

    val actual = curv.decodePoint(array2).asInstanceOf[Curve25519Point]

    //element.get.equals(point) shouldBe true*/

  }

/*  private def CreatePoint = {

    val msg = "sample"

    val msgArray = msg.toArray.map(x => x.asInstanceOf[Byte])

    val el: Try[Curve25519Point] = scapi.sigma.Curve25519.encodeByteArrayToGroupElement(msgArray)
    el.isSuccess should be(true)

    (msgArray, el.get)
  }*/

  override def objects = Table(
    ("object", "bytes"),
  )

  tableRoundTripTest("Not: Serializer round trip")
  tablePredefinedBytesTest("Not: deserialize from predefined bytes")
}