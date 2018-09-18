package sigmastate.utxo

import org.ergoplatform.ErgoBox.{R6, R8}
import org.ergoplatform.{ErgoBox, ErgoLikeContext, ErgoLikeInterpreter, Self}
import sigmastate.SCollection.SByteArray
import sigmastate.Values._
import sigmastate._
import sigmastate.helpers.{ErgoLikeProvingInterpreter, SigmaTestingCommons}
import sigmastate.lang.Terms._
import sigmastate.lang.exceptions.{InvalidType, OptionUnwrapNone}
import sigmastate.utxo.GetVar._

class BasicOpsSpecification extends SigmaTestingCommons {
  private val reg1 = ErgoBox.nonMandatoryRegisters.head
  val intVar1 = 1.toByte
  val intVar2 = 2.toByte
  val byteVar1 = 3.toByte
  val byteVar2 = 4.toByte
  val bigIntVar1 = 5.toByte
  val bigIntVar2 = 6.toByte
  val bigIntVar3 = 7.toByte
  val byteVar3 = 8.toByte
  val booleanVar = 9.toByte
  val propVar1 = 10.toByte
  val propVar2 = 11.toByte
  val lastExtVar = propVar2

  val ext = Seq(
    (intVar1, IntConstant(1)), (intVar2, IntConstant(2)),
    (byteVar1, ByteConstant(1)), (byteVar2, ByteConstant(2)),
    (bigIntVar1, BigIntConstant(BigInt(10).underlying())), (bigIntVar2, BigIntConstant(BigInt(20).underlying())),
    (booleanVar, TrueLeaf))
  val env = Map(
    "intVar1" -> intVar1, "intVar2" -> intVar2,
    "byteVar1" -> byteVar1, "byteVar2" -> byteVar2, "byteVar3" -> byteVar3,
    "bigIntVar1" -> bigIntVar1, "bigIntVar2" -> bigIntVar2, "bigIntVar3" -> bigIntVar3,
    "trueVar" -> booleanVar,
    "proofVar1" -> propVar1,
    "proofVar2" -> propVar2
    )

  def test(env: Map[String, Any],
           ext: Seq[(Byte, EvaluatedValue[_ <: SType])],
           script: String, propExp: Value[SBoolean.type],
      onlyPositive: Boolean = false) = {
    val prover = new ErgoLikeProvingInterpreter() {
      override lazy val contextExtenders: Map[Byte, EvaluatedValue[_ <: SType]] = {
        val p1 = dlogSecrets(0).publicImage
        val p2 = dlogSecrets(1).publicImage
        (ext ++ Seq(propVar1 -> SigmaPropConstant(p1), propVar2 -> SigmaPropConstant(p2))).toMap
      }
    }

    val prop = compile(env, script).asBoolValue
    prop shouldBe propExp

    val p3 = prover.dlogSecrets(2).publicImage
    val outputToSpend = ErgoBox(10, prop, additionalRegisters = Map(reg1 -> SigmaPropConstant(p3)))

    val ctx = ErgoLikeContext.dummy(outputToSpend)

    val pr = prover.prove(prop, ctx, fakeMessage).get

    val ctxExt = ctx.withExtension(pr.extension)

    val verifier = new ErgoLikeInterpreter
    if (!onlyPositive)
      verifier.verify(prop, ctx, pr.proof, fakeMessage).map(_._1).getOrElse(false) shouldBe false //context w/out extensions
    verifier.verify(prop, ctxExt, pr.proof, fakeMessage).get._1 shouldBe true
  }

  property("Relation operations") {
    test(env, ext,
      "{ allOf(Array(getVar[Boolean](trueVar).get, true, true)) }",
      AND(GetVarBoolean(booleanVar).get, TrueLeaf, TrueLeaf)
    )
    test(env, ext,
      "{ anyOf(Array(getVar[Boolean](trueVar).get, true, false)) }",
      OR(GetVarBoolean(booleanVar).get, TrueLeaf, FalseLeaf)
    )
    test(env, ext,
      "{ getVar[Int](intVar2).get > getVar[Int](intVar1).get && getVar[Int](intVar1).get < getVar[Int](intVar2).get }",
      AND(GT(GetVarInt(intVar2).get, GetVarInt(intVar1).get),
        LT(GetVarInt(intVar1).get, GetVarInt(intVar2).get))
    )
    test(env, ext,
      "{ getVar[Int](intVar2).get >= getVar[Int](intVar1).get && getVar[Int](intVar1).get <= getVar[Int](intVar2).get }",
      AND(GE(GetVarInt(intVar2).get, GetVarInt(intVar1).get),
        LE(GetVarInt(intVar1).get, GetVarInt(intVar2).get))
    )
    test(env, ext,
      "{ getVar[Byte](byteVar2).get > getVar[Byte](byteVar1).get && getVar[Byte](byteVar1).get < getVar[Byte](byteVar2).get }",
      AND(GT(GetVarByte(byteVar2).get, GetVarByte(byteVar1).get),
        LT(GetVarByte(byteVar1).get, GetVarByte(byteVar2).get))
    )
    test(env, ext,
      "{ getVar[Byte](byteVar2).get >= getVar[Byte](byteVar1).get && getVar[Byte](byteVar1).get <= getVar[Byte](byteVar2).get }",
      AND(GE(GetVarByte(byteVar2).get, GetVarByte(byteVar1).get),
        LE(GetVarByte(byteVar1).get, GetVarByte(byteVar2).get))
    )
    test(env, ext,
      "{ getVar[BigInt](bigIntVar2).get > getVar[BigInt](bigIntVar1).get && getVar[BigInt](bigIntVar1).get < getVar[BigInt](bigIntVar2).get }",
      AND(
        GT(GetVarBigInt(bigIntVar2).get, GetVarBigInt(bigIntVar1).get),
        LT(GetVarBigInt(bigIntVar1).get, GetVarBigInt(bigIntVar2).get))
    )
    test(env, ext,
      "{ getVar[BigInt](bigIntVar2).get >= getVar[BigInt](bigIntVar1).get && getVar[BigInt](bigIntVar1).get <= getVar[BigInt](bigIntVar2).get }",
      AND(
        GE(GetVarBigInt(bigIntVar2).get, GetVarBigInt(bigIntVar1).get),
        LE(GetVarBigInt(bigIntVar1).get, GetVarBigInt(bigIntVar2).get))
    )
  }

  property("SigmaProp operations") {
    test(env, ext,
      "{ getVar[SigmaProp](proofVar1).get.isValid }",
      GetVarSigmaProp(propVar1).get.isValid
    )
    test(env, ext,
      "{ getVar[SigmaProp](proofVar1).get || getVar[SigmaProp](proofVar2).get }",
      OR(GetVarSigmaProp(propVar1).get.isValid, GetVarSigmaProp(propVar2).get.isValid)
    )
    test(env, ext,
      "{ getVar[SigmaProp](proofVar1).get && getVar[SigmaProp](proofVar2).get }",
      AND(GetVarSigmaProp(propVar1).get.isValid, GetVarSigmaProp(propVar2).get.isValid)
    )
    test(env, ext,
      "{ getVar[SigmaProp](proofVar1).get.isValid && getVar[SigmaProp](proofVar2).get }",
      AND(GetVarSigmaProp(propVar1).get.isValid, GetVarSigmaProp(propVar2).get.isValid)
    )
    test(env, ext,
      "{ getVar[SigmaProp](proofVar1).get && getVar[Int](intVar1).get == 1 }",
      AND(GetVarSigmaProp(propVar1).get.isValid, EQ(GetVarInt(intVar1).get, 1))
    )
    test(env, ext,
      "{ getVar[Int](intVar1).get == 1 || getVar[SigmaProp](proofVar1).get }",
      OR(EQ(GetVarInt(intVar1).get, 1), GetVarSigmaProp(propVar1).get.isValid)
    )
    test(env, ext,
      "{ SELF.R4[SigmaProp].get.isValid }",
      ExtractRegisterAs[SSigmaProp.type](Self, reg1).get.isValid,
      true
    )
    test(env, ext,
      "{ SELF.R4[SigmaProp].get && getVar[SigmaProp](proofVar1).get}",
      AND(ExtractRegisterAs[SSigmaProp.type](Self, reg1).get.isValid, GetVarSigmaProp(propVar1).get.isValid),
      true
    )
    test(env, ext,
      "{ allOf(Array(SELF.R4[SigmaProp].get, getVar[SigmaProp](proofVar1).get))}",
      AND(ExtractRegisterAs[SSigmaProp.type](Self, reg1).get.isValid, GetVarSigmaProp(propVar1).get.isValid),
      true
    )
    test(env, ext,
      "{ anyOf(Array(SELF.R4[SigmaProp].get, getVar[SigmaProp](proofVar1).get))}",
      OR(ExtractRegisterAs[SSigmaProp.type](Self, reg1).get.isValid, GetVarSigmaProp(propVar1).get.isValid),
      true
    )
    test(env, ext,
      "{ Array(SELF.R4[SigmaProp].get, getVar[SigmaProp](proofVar1).get).forall({ (p: SigmaProp) => p.isValid }) }",
      ForAll(ConcreteCollection(ExtractRegisterAs[SSigmaProp.type](Self, reg1).get, GetVarSigmaProp(propVar1).get),
        21, SigmaPropIsValid(TaggedSigmaProp(21))),
      true
    )
    test(env, ext,
      "{ SELF.R4[SigmaProp].get.propBytes != getVar[SigmaProp](proofVar1).get.propBytes }",
      NEQ(ExtractRegisterAs[SSigmaProp.type](Self, reg1).get.propBytes, GetVarSigmaProp(propVar1).get.propBytes),
      true
    )
  }

  property("Arith operations") {
    test(env, ext,
      "{ getVar[Int](intVar2).get * 2 + getVar[Int](intVar1).get == 5 }",
      EQ(Plus(Multiply(GetVarInt(intVar2).get, IntConstant(2)), GetVarInt(intVar1).get), IntConstant(5))
    )

    test(env, ext :+ (bigIntVar3 -> BigIntConstant(50)),
      "{ getVar[BigInt](bigIntVar2).get * 2 + getVar[BigInt](bigIntVar1).get == getVar[BigInt](bigIntVar3).get }",
      EQ(
        Plus(Multiply(GetVarBigInt(bigIntVar2).get, BigIntConstant(2)),
          GetVarBigInt(bigIntVar1).get),
        GetVarBigInt(bigIntVar3).get)
    )
    test(env, ext :+ (byteVar3 -> ByteConstant(5)),
      "{ getVar[Byte](byteVar2).get * 2.toByte + getVar[Byte](byteVar1).get == 5.toByte }",
      EQ(
        Plus(Multiply(GetVarByte(byteVar2).get, ByteConstant(2)),
          GetVarByte(byteVar1).get), ByteConstant(5))
    )
    test(env, ext,
      "{ getVar[Int](intVar2).get / 2 + getVar[Int](intVar1).get == 2 }",
      EQ(Plus(Divide(GetVarInt(intVar2).get, IntConstant(2)), GetVarInt(intVar1).get), IntConstant(2))
    )
    test(env, ext,
      "{ getVar[Int](intVar2).get % 2 + getVar[Int](intVar1).get == 1 }",
      EQ(Plus(Modulo(GetVarInt(intVar2).get, IntConstant(2)), GetVarInt(intVar1).get), IntConstant(1))
    )
  }

  property("Tuple operations") {
    test(env, ext,
      "{ (getVar[Int](intVar1).get, getVar[Int](intVar2).get)._1 == 1 }",
      EQ(SelectField(Tuple(GetVarInt(intVar1).get, GetVarInt(intVar2).get), 1), IntConstant(1))
    )
    test(env, ext,
      "{ (getVar[Int](intVar1).get, getVar[Int](intVar2).get)._2 == 2 }",
      EQ(SelectField(Tuple(GetVarInt(intVar1).get, GetVarInt(intVar2).get), 2), IntConstant(2))
    )
    test(env, ext,
      """{ val p = (getVar[Int](intVar1).get, getVar[Int](intVar2).get)
        |  val res = p._1 + p._2
        |  res == 3 }""".stripMargin,
      {
        val p = Tuple(GetVarInt(intVar1).get, GetVarInt(intVar2).get)
        val res = Plus(SelectField(p, 1).asIntValue, SelectField(p, 2).asIntValue)
        EQ(res, IntConstant(3))
      }
    )
  }

  property("Tuple as Collection operations") {
    test(env, ext,
    """{ val p = (getVar[Int](intVar1).get, getVar[Byte](byteVar2).get)
     |  p.size == 2 }""".stripMargin,
    {
      val p = Tuple(GetVarInt(intVar1).get, GetVarByte(byteVar2).get)
      EQ(SizeOf(p), IntConstant(2))
    })
    test(env, ext,
    """{ val p = (getVar[Int](intVar1).get, getVar[Byte](byteVar2).get)
     |  p(0) == 1 }""".stripMargin,
    {
      val p = Tuple(GetVarInt(intVar1).get, GetVarByte(byteVar2).get)
      EQ(SelectField(p, 1), IntConstant(1))
    })

    val dataVar = (lastExtVar + 1).toByte
    val data = Array(Array[Any](Array[Byte](1,2,3), 10L))
    val env1 = env + ("dataVar" -> dataVar)
    val dataType = SCollection(STuple(SCollection(SByte), SLong))
    val ext1 = ext :+ (dataVar, Constant[SCollection[STuple]](data, dataType))
    test(env1, ext1,
      """{
        |  val data = getVar[Array[(Array[Byte], Long)]](dataVar).get
        |  data.size == 1
        |}""".stripMargin,
      {
        val data = GetVar(dataVar, dataType).get
        EQ(SizeOf(data), IntConstant(1))
      }
    )
    test(env1, ext1,
      """{
        |  val data = getVar[Array[(Array[Byte], Long)]](dataVar).get
        |  data.exists({ (p: (Array[Byte], Long)) => p._2 == 10L })
        |}""".stripMargin,
      {
        val data = GetVar(dataVar, dataType).get
        Exists(data, 21, EQ(SelectField(TaggedVariable(21, STuple(SCollection(SByte), SLong)), 2), LongConstant(10)))
      }
    )
    test(env1, ext1,
      """{
        |  val data = getVar[Array[(Array[Byte], Long)]](dataVar).get
        |  data.forall({ (p: (Array[Byte], Long)) => p._1.size > 0 })
        |}""".stripMargin,
      {
        val data = GetVar(dataVar, dataType).get
        val p = TaggedVariable(21, STuple(SCollection(SByte), SLong))
        ForAll(data, 21, GT(SizeOf(SelectField(p, 1).asValue[SByteArray]), IntConstant(0)))
      }
    )
    test(env1, ext1,
      """{
        |  val data = getVar[Array[(Array[Byte], Long)]](dataVar).get
        |  data.map({ (p: (Array[Byte], Long)) => (p._2, p._1)}).size == 1
        |}""".stripMargin,
      {
        val data = GetVar(dataVar, dataType).get
        val p = TaggedVariable(21, STuple(SCollection(SByte), SLong))
        val swapped = MapCollection(data, 21, Tuple(SelectField(p, 2), SelectField(p, 1))).asCollection[STuple]
        EQ(SizeOf(swapped), IntConstant(1))
      }
    )

// TODO uncomment after operations over Any are implemented
//    test(env, ext,
//    """{ val p = (getVar[Int](intVar1).get, getVar[Byte](byteVar2).get)
//     |  p.getOrElse(2, 3).isInstanceOf[Int] }""".stripMargin,
//    {
//      val p = Tuple(GetVarInt(intVar1).get, GetVarByte(byteVar2).get)
//      EQ(ByIndex[SAny.type](p, IntConstant(2), Some(IntConstant(3).asValue[SAny.type])), IntConstant(3))
//    })
  }

  property("GetVar") {
    test(env, ext,
      "{ getVar[Int](intVar2).get == 2 }",
      EQ(GetVarInt(intVar2).get, IntConstant(2))
    )
    an[InvalidType] should be thrownBy test(env, ext,
      "{ getVar[Int](proofVar1).get == 2 }",
      EQ(GetVarInt(propVar1).get, IntConstant(2))
    )
  }

  property("ExtractRegisterAs") {
    test(env, ext,
      "{ SELF.R4[SigmaProp].get.isValid }",
      ExtractRegisterAs[SSigmaProp.type](Self, reg1).get.isValid,
      true
    )
    an[InvalidType] should be thrownBy test(env, ext,
      "{ SELF.R4[Int].get == 1 }",
      EQ(ExtractRegisterAs[SInt.type](Self, reg1).get, IntConstant(1)),
      true
    )
  }

  property("OptionGet success (SomeValue)") {
    test(env, ext,
      "{ getVar[Int](intVar2).get == 2 }",
      EQ(GetVarInt(intVar2).get, IntConstant(2))
    )
  }

  property("OptionGet fail (NoneValue)") {
    an[OptionUnwrapNone] should be thrownBy test(env, ext,
      "{ getVar[Int](99).get == 2 }",
      EQ(GetVarInt(99).get, IntConstant(2))
    )
    an[OptionUnwrapNone] should be thrownBy test(env, ext,
      "{ SELF.R8[SigmaProp].get.propBytes != getVar[SigmaProp](proofVar1).get.propBytes }",
      NEQ(ExtractRegisterAs[SSigmaProp.type](Self, R8).get.propBytes, GetVarSigmaProp(propVar1).get.propBytes),
      true
    )
  }

  property("OptionGetOrElse") {
    test(env, ext,
      "{ SELF.R6[Int].getOrElse(3) == 3 }",
      EQ(ExtractRegisterAs[SInt.type](Self, R6).getOrElse(IntConstant(3)), IntConstant(3)),
      true
    )
  }
}
