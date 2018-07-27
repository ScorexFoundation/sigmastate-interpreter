package sigmastate.utxo

import org.ergoplatform.{ErgoLikeContext, ErgoBox, ErgoLikeInterpreter, Self}
import sigmastate.SCollection.SByteArray
import sigmastate.Values._
import sigmastate._
import sigmastate.helpers.{ErgoLikeProvingInterpreter, SigmaTestingCommons}
import sigmastate.lang.Terms._

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
  val proofVar1 = 10.toByte
  val proofVar2 = 11.toByte
  val lastExtVar = proofVar2

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
    "proofVar1" -> proofVar1,
    "proofVar2" -> proofVar2
    )

  def test(env: Map[String, Any],
           ext: Seq[(Byte, EvaluatedValue[_ <: SType])],
           script: String, propExp: Value[SBoolean.type],
      onlyPositive: Boolean = false) = {
    val prover = new ErgoLikeProvingInterpreter() {
      override lazy val contextExtenders: Map[Byte, EvaluatedValue[_ <: SType]] = {
        val p1 = dlogSecrets(0).publicImage
        val p2 = dlogSecrets(1).publicImage
        (ext ++ Seq(proofVar1 -> SigmaPropConstant(p1), proofVar2 -> SigmaPropConstant(p2))).toMap
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
      "{ allOf(Array(getVar[Boolean](trueVar), true, true)) }",
      AND(TaggedBoolean(booleanVar), TrueLeaf, TrueLeaf)
    )
    test(env, ext,
      "{ anyOf(Array(getVar[Boolean](trueVar), true, false)) }",
      OR(TaggedBoolean(booleanVar), TrueLeaf, FalseLeaf)
    )
    test(env, ext,
      "{ getVar[Int](intVar2) > getVar[Int](intVar1) && getVar[Int](intVar1) < getVar[Int](intVar2) }",
      AND(GT(TaggedInt(intVar2), TaggedInt(intVar1)), LT(TaggedInt(intVar1), TaggedInt(intVar2)))
    )
    test(env, ext,
      "{ getVar[Int](intVar2) >= getVar[Int](intVar1) && getVar[Int](intVar1) <= getVar[Int](intVar2) }",
      AND(GE(TaggedInt(intVar2), TaggedInt(intVar1)), LE(TaggedInt(intVar1), TaggedInt(intVar2)))
    )
    test(env, ext,
      "{ getVar[Byte](byteVar2) > getVar[Byte](byteVar1) && getVar[Byte](byteVar1) < getVar[Byte](byteVar2) }",
      AND(GT(TaggedByte(byteVar2), TaggedByte(byteVar1)), LT(TaggedByte(byteVar1), TaggedByte(byteVar2)))
    )
    test(env, ext,
      "{ getVar[Byte](byteVar2) >= getVar[Byte](byteVar1) && getVar[Byte](byteVar1) <= getVar[Byte](byteVar2) }",
      AND(GE(TaggedByte(byteVar2), TaggedByte(byteVar1)), LE(TaggedByte(byteVar1), TaggedByte(byteVar2)))
    )
    test(env, ext,
      "{ getVar[BigInt](bigIntVar2) > getVar[BigInt](bigIntVar1) && getVar[BigInt](bigIntVar1) < getVar[BigInt](bigIntVar2) }",
      AND(GT(TaggedBigInt(bigIntVar2), TaggedBigInt(bigIntVar1)), LT(TaggedBigInt(bigIntVar1), TaggedBigInt(bigIntVar2)))
    )
    test(env, ext,
      "{ getVar[BigInt](bigIntVar2) >= getVar[BigInt](bigIntVar1) && getVar[BigInt](bigIntVar1) <= getVar[BigInt](bigIntVar2) }",
      AND(GE(TaggedBigInt(bigIntVar2), TaggedBigInt(bigIntVar1)), LE(TaggedBigInt(bigIntVar1), TaggedBigInt(bigIntVar2)))
    )
  }

  property("Proof operations") {
    test(env, ext,
      "{ getVar[Proof](proofVar1).isValid }",
      TaggedProof(proofVar1).isValid
    )
    test(env, ext,
      "{ getVar[Proof](proofVar1) || getVar[Proof](proofVar2) }",
      OR(TaggedProof(proofVar1).isValid, TaggedProof(proofVar2).isValid)
    )
    test(env, ext,
      "{ getVar[Proof](proofVar1) && getVar[Proof](proofVar2) }",
      AND(TaggedProof(proofVar1).isValid, TaggedProof(proofVar2).isValid)
    )
    test(env, ext,
      "{ getVar[Proof](proofVar1).isValid && getVar[Proof](proofVar2) }",
      AND(TaggedProof(proofVar1).isValid, TaggedProof(proofVar2).isValid)
    )
    test(env, ext,
      "{ getVar[Proof](proofVar1) && getVar[Int](intVar1) == 1 }",
      AND(TaggedProof(proofVar1).isValid, EQ(TaggedInt(intVar1), 1))
    )
    test(env, ext,
      "{ getVar[Int](intVar1) == 1 || getVar[Proof](proofVar1) }",
      OR(EQ(TaggedInt(intVar1), 1), TaggedProof(proofVar1).isValid)
    )
    test(env, ext,
      "{ SELF.R4[Proof].value.isValid }",
      ExtractRegisterAs[SSigmaProp.type](Self, reg1).isValid,
      true
    )
    test(env, ext,
      "{ SELF.R4[Proof].value && getVar[Proof](proofVar1)}",
      AND(ExtractRegisterAs[SSigmaProp.type](Self, reg1).isValid, TaggedProof(proofVar1).isValid),
      true
    )
    test(env, ext,
      "{ allOf(Array(SELF.R4[Proof].value, getVar[Proof](proofVar1)))}",
      AND(ExtractRegisterAs[SSigmaProp.type](Self, reg1).isValid, TaggedProof(proofVar1).isValid),
      true
    )
    test(env, ext,
      "{ anyOf(Array(SELF.R4[Proof].value, getVar[Proof](proofVar1)))}",
      OR(ExtractRegisterAs[SSigmaProp.type](Self, reg1).isValid, TaggedProof(proofVar1).isValid),
      true
    )
    test(env, ext,
      "{ Array(SELF.R4[Proof].value, getVar[Proof](proofVar1)).forall(fun (p: Proof) = p.isValid) }",
      ForAll(ConcreteCollection(ExtractRegisterAs[SSigmaProp.type](Self, reg1), TaggedProof(proofVar1)),
        21, SigmaPropIsValid(TaggedProof(21))),
      true
    )
    test(env, ext,
      "{ SELF.R4[Proof].value.propBytes != getVar[Proof](proofVar1).propBytes }",
      NEQ(ExtractRegisterAs[SSigmaProp.type](Self, reg1).propBytes, TaggedProof(proofVar1).propBytes),
      true
    )
  }

  property("Arith operations") {
    test(env, ext,
      "{ getVar[Int](intVar2) * 2 + getVar[Int](intVar1) == 5 }",
      EQ(Plus(Multiply(TaggedInt(intVar2), IntConstant(2)), TaggedInt(intVar1)), IntConstant(5))
    )
    test(env, ext :+ (bigIntVar3 -> BigIntConstant(50)),
      "{ getVar[BigInt](bigIntVar2) * 2 + getVar[BigInt](bigIntVar1) == getVar[BigInt](bigIntVar3) }",
      EQ(Plus(Multiply(TaggedBigInt(bigIntVar2), BigIntConstant(2)), TaggedBigInt(bigIntVar1)), TaggedBigInt(bigIntVar3))
    )
    test(env, ext :+ (byteVar3 -> ByteConstant(5)),
      "{ getVar[Byte](byteVar2) * intToByte(2) + getVar[Byte](byteVar1) == intToByte(5) }",
      EQ(Plus(Multiply(TaggedByte(byteVar2), IntToByte(IntConstant(2))), TaggedByte(byteVar1)), IntToByte(IntConstant(5)))
    )
    test(env, ext,
      "{ getVar[Int](intVar2) / 2 + getVar[Int](intVar1) == 2 }",
      EQ(Plus(Divide(TaggedInt(intVar2), IntConstant(2)), TaggedInt(intVar1)), IntConstant(2))
    )
    test(env, ext,
      "{ getVar[Int](intVar2) % 2 + getVar[Int](intVar1) == 1 }",
      EQ(Plus(Modulo(TaggedInt(intVar2), IntConstant(2)), TaggedInt(intVar1)), IntConstant(1))
    )
  }

  property("Tuple operations") {
    test(env, ext,
      "{ (getVar[Int](intVar1), getVar[Int](intVar2))._1 == 1 }",
      EQ(SelectField(Tuple(TaggedInt(intVar1), TaggedInt(intVar2)), 1), IntConstant(1))
    )
    test(env, ext,
      "{ (getVar[Int](intVar1), getVar[Int](intVar2))._2 == 2 }",
      EQ(SelectField(Tuple(TaggedInt(intVar1), TaggedInt(intVar2)), 2), IntConstant(2))
    )
    test(env, ext,
      """{ let p = (getVar[Int](intVar1), getVar[Int](intVar2))
        |  let res = p._1 + p._2
        |  res == 3 }""".stripMargin,
      {
        val p = Tuple(TaggedInt(intVar1), TaggedInt(intVar2))
        val res = Plus(SelectField(p, 1).asIntValue, SelectField(p, 2).asIntValue)
        EQ(res, IntConstant(3))
      }
    )
  }

  property("Tuple as Collection operations") {
    test(env, ext,
    """{ let p = (getVar[Int](intVar1), getVar[Byte](byteVar2))
     |  p.size == 2 }""".stripMargin,
    {
      val p = Tuple(TaggedInt(intVar1), TaggedByte(byteVar2))
      EQ(SizeOf(p), IntConstant(2))
    })
    test(env, ext,
    """{ let p = (getVar[Int](intVar1), getVar[Byte](byteVar2))
     |  p(0) == 1 }""".stripMargin,
    {
      val p = Tuple(TaggedInt(intVar1), TaggedByte(byteVar2))
      EQ(SelectField(p, 1), IntConstant(1))
    })

    val dataVar = (lastExtVar + 1).toByte
    val data = Array(Array[Any](Array[Byte](1,2,3), 10L))
    val env1 = env + ("dataVar" -> dataVar)
    val dataType = SCollection(STuple(SCollection(SByte), SLong))
    val ext1 = ext :+ (dataVar, Constant[SCollection[STuple]](data, dataType))
    test(env1, ext1,
      """{
        |  let data = getVar[Array[(Array[Byte], Long)]](dataVar)
        |  data.size == 1
        |}""".stripMargin,
      {
        val data = TaggedVariable(dataVar, dataType)
        EQ(SizeOf(data), IntConstant(1))
      }
    )
    test(env1, ext1,
      """{
        |  let data = getVar[Array[(Array[Byte], Long)]](dataVar)
        |  data.exists(fun (p: (Array[Byte], Long)) = p._2 == 10L)
        |}""".stripMargin,
      {
        val data = TaggedVariable(dataVar, dataType)
        Exists(data, 21, EQ(SelectField(TaggedVariable(21, STuple(SCollection(SByte), SLong)), 2), LongConstant(10)))
      }
    )
    test(env1, ext1,
      """{
        |  let data = getVar[Array[(Array[Byte], Long)]](dataVar)
        |  data.forall(fun (p: (Array[Byte], Long)) = p._1.size > 0)
        |}""".stripMargin,
      {
        val data = TaggedVariable(dataVar, dataType)
        val p = TaggedVariable(21, STuple(SCollection(SByte), SLong))
        ForAll(data, 21, GT(SizeOf(SelectField(p, 1).asValue[SByteArray]), IntConstant(0)))
      }
    )
    test(env1, ext1,
      """{
        |  let data = getVar[Array[(Array[Byte], Long)]](dataVar)
        |  data.map(fun (p: (Array[Byte], Long)) = (p._2, p._1)).size == 1
        |}""".stripMargin,
      {
        val data = TaggedVariable(dataVar, dataType)
        val p = TaggedVariable(21, STuple(SCollection(SByte), SLong))
        val swapped = MapCollection(data, 21, Tuple(SelectField(p, 2), SelectField(p, 1))).asCollection[STuple]
        EQ(SizeOf(swapped), IntConstant(1))
      }
    )

// TODO uncomment after operations over Any are implemented
//    test(env, ext,
//    """{ let p = (getVar[Int](intVar1), getVar[Byte](byteVar2))
//     |  p.getOrElse(2, 3).isInstanceOf[Int] }""".stripMargin,
//    {
//      val p = Tuple(TaggedInt(intVar1), TaggedByte(byteVar2))
//      EQ(ByIndex[SAny.type](p, IntConstant(2), Some(IntConstant(3).asValue[SAny.type])), IntConstant(3))
//    })
  }
}
