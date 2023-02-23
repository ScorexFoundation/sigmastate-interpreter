package org.ergoplatform.sdk

import sigmastate.Values._
import sigmastate.eval.CBigInt
import sigmastate.lang.TransformingSigmaBuilder.mkConstant
import special.sigma.ContractsTestkit
import sigmastate.serialization.SerializationSpecification
import sigmastate.SType
import sigmastate._
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import sigmastate.helpers.NegativeTesting

import java.math.BigInteger
import sigmastate.EQ
import org.scalatest.compatible.Assertion
import sigmastate.serialization.SigmaSerializer

class ContractTemplateSpecification extends SerializationSpecification 
  with ScalaCheckPropertyChecks 
  with ContractsTestkit 
  with NegativeTesting
  with CrossVersionProps {
  object JsonCodecs extends JsonCodecs

  private def contractTemplateNameInTests: String = "TestContractTemplate"
  private def contractTemplateDescriptionInTests: String = "TestContractTemplateDescription"

  def jsonRoundtrip[T <: SType](obj: ContractTemplate) = {
    val json = ContractTemplate.jsonEncoder.encoder(obj)
    val res = ContractTemplate.jsonEncoder.decoder(json.hcursor).right.get
    res shouldBe obj
  }
  
  def serializationRoundTrip(template: ContractTemplate): Assertion = {
    val w = SigmaSerializer.startWriter()
    ContractTemplate.serializer.serialize(template, w)
    val bytes = w.toBytes
    val r = SigmaSerializer.startReader(bytes)
    val res2 = ContractTemplate.serializer.parse(r)
    res2 shouldEqual template
  }

  private def createParameter(name: String, placeholder: Int): Parameter = {
    Parameter(
      name,
      s"${name}_description",
      placeholder
    )
  }

  private def createContractTemplate(constTypes: IndexedSeq[SType],
                                     constValues: Option[IndexedSeq[Option[SType#WrappedType]]],
                                     parameters: IndexedSeq[Parameter],
                                     expressionTree: SigmaPropValue): ContractTemplate = {
    ContractTemplate(
      contractTemplateNameInTests,
      contractTemplateDescriptionInTests,
      constTypes,
      constValues,
      parameters,
      expressionTree
    )
  }

  property("unequal length of constTypes and constValues") {
    assertExceptionThrown(
      createContractTemplate(
        IndexedSeq(SType.typeByte, SType.typeByte, SType.typeByte).asInstanceOf[IndexedSeq[SType]],
        Some(IndexedSeq(Some(10.toByte), Some(20.toByte)).asInstanceOf[IndexedSeq[Option[SType#WrappedType]]]),
        IndexedSeq(
          createParameter("p1", 0),
          createParameter("p2", 1),
          createParameter("p3", 2)),
        EQ(Plus(ConstantPlaceholder(0, SType.typeByte),
          ConstantPlaceholder(1, SType.typeByte)),
          ConstantPlaceholder(2, SType.typeByte)).toSigmaProp
      ),
      t =>
        t.isInstanceOf[IllegalArgumentException] &&
        t.getMessage.contains("constValues must be empty or of same length as constTypes. Got 2, expected 3"))
  }

  property("more parameters than constants") {
    assertExceptionThrown(
      createContractTemplate(
        IndexedSeq(SType.typeByte, SType.typeByte, SType.typeByte).asInstanceOf[IndexedSeq[SType]],
        Some(IndexedSeq(Some(10.toByte), Some(20.toByte), Some(30.toByte)).asInstanceOf[IndexedSeq[Option[SType#WrappedType]]]),
        IndexedSeq(
          createParameter("p1", 0),
          createParameter("p2", 1),
          createParameter("p3", 2),
          createParameter("p4", 3)),
        EQ(Plus(ConstantPlaceholder(0, SType.typeByte),
          ConstantPlaceholder(1, SType.typeByte)),
          ConstantPlaceholder(2, SType.typeByte)).toSigmaProp
      ),
      t =>
        t.isInstanceOf[IllegalArgumentException] &&
          t.getMessage.contains("number of parameters must be <= number of constants"))
  }

  property("invalid parameter placeholder") {
    assertExceptionThrown(
      createContractTemplate(
        IndexedSeq(SType.typeByte, SType.typeByte, SType.typeByte).asInstanceOf[IndexedSeq[SType]],
        Some(IndexedSeq(Some(10.toByte), Some(20.toByte), Some(30.toByte)).asInstanceOf[IndexedSeq[Option[SType#WrappedType]]]),
        IndexedSeq(
          createParameter("p1", 0),
          createParameter("p2", 1),
          createParameter("p3", 100)),
        EQ(Plus(ConstantPlaceholder(0, SType.typeByte),
          ConstantPlaceholder(1, SType.typeByte)),
          ConstantPlaceholder(2, SType.typeByte)).toSigmaProp
      ),
      t =>
        t.isInstanceOf[IllegalArgumentException] &&
          t.getMessage.contains("parameter placeholder must be in range [0, 3)"))
  }

  property("duplicate parameter placeholder") {
    assertExceptionThrown(
      createContractTemplate(
        IndexedSeq(SType.typeByte, SType.typeByte, SType.typeByte).asInstanceOf[IndexedSeq[SType]],
        Some(IndexedSeq(Some(10.toByte), Some(20.toByte), Some(30.toByte)).asInstanceOf[IndexedSeq[Option[SType#WrappedType]]]),
        IndexedSeq(
          createParameter("p1", 0),
          createParameter("p2", 1),
          createParameter("p3", 1)),
        EQ(Plus(ConstantPlaceholder(0, SType.typeByte),
          ConstantPlaceholder(1, SType.typeByte)),
          ConstantPlaceholder(2, SType.typeByte)).toSigmaProp
      ),
      t =>
        t.isInstanceOf[IllegalArgumentException] &&
          t.getMessage.contains("multiple parameters point to the same placeholder"))
  }

  property("duplicate parameter names") {
    assertExceptionThrown(
      createContractTemplate(
        IndexedSeq(SType.typeByte, SType.typeByte, SType.typeByte).asInstanceOf[IndexedSeq[SType]],
        Some(IndexedSeq(Some(10.toByte), Some(20.toByte), Some(30.toByte)).asInstanceOf[IndexedSeq[Option[SType#WrappedType]]]),
        IndexedSeq(
          createParameter("duplicate_name", 0),
          createParameter("p2", 1),
          createParameter("duplicate_name", 2)),
        EQ(Plus(ConstantPlaceholder(0, SType.typeByte),
          ConstantPlaceholder(1, SType.typeByte)),
          ConstantPlaceholder(2, SType.typeByte)).toSigmaProp
      ),
      t =>
        t.isInstanceOf[IllegalArgumentException] &&
          t.getMessage.contains("parameter names must be unique. Found duplicate parameters with name duplicate_name"))
  }

  property("placeholder without default value and parameter") {
    assertExceptionThrown(
      createContractTemplate(
        IndexedSeq(SType.typeByte, SType.typeByte, SType.typeByte).asInstanceOf[IndexedSeq[SType]],
        Some(IndexedSeq(None, Some(20.toByte), Some(30.toByte)).asInstanceOf[IndexedSeq[Option[SType#WrappedType]]]),
        IndexedSeq(
          createParameter("p2", 1),
          createParameter("p3", 2)),
        EQ(Plus(ConstantPlaceholder(0, SType.typeByte),
          ConstantPlaceholder(1, SType.typeByte)),
          ConstantPlaceholder(2, SType.typeByte)).toSigmaProp
      ),
      t =>
        t.isInstanceOf[IllegalArgumentException] &&
          t.getMessage.contains("placeholder 0 does not have a default value and absent from parameter as well"))
  }

  property("applyTemplate") {
    val parameters = IndexedSeq(
      createParameter("p1", 0),
      createParameter("p2", 1),
      createParameter("p3", 2))
    val expressionTrees = IndexedSeq(
      EQ(Plus(ConstantPlaceholder(0, SType.typeByte),
        ConstantPlaceholder(1, SType.typeByte)),
        ConstantPlaceholder(2, SType.typeByte)).toSigmaProp,
      EQ(Plus(ConstantPlaceholder(0, SType.typeInt),
        ConstantPlaceholder(1, SType.typeInt)),
        ConstantPlaceholder(2, SType.typeInt)).toSigmaProp,
      EQ(Plus(CBigInt(BigInteger.valueOf(10L)), BigIntConstant(20L)), BigIntConstant(30L)).toSigmaProp
    )
    val templates = Seq(
      createContractTemplate(
        IndexedSeq(SType.typeByte, SType.typeByte, SType.typeByte).asInstanceOf[IndexedSeq[SType]],
        Some(IndexedSeq(Some(10.toByte), Some(20.toByte), Some(30.toByte)).asInstanceOf[IndexedSeq[Option[SType#WrappedType]]]),
        parameters,
        expressionTrees(0)
      ),
      createContractTemplate(
        IndexedSeq(SType.typeInt, SType.typeInt, SType.typeInt).asInstanceOf[IndexedSeq[SType]],
        Some(IndexedSeq(Some(10), None, Some(30)).asInstanceOf[IndexedSeq[Option[SType#WrappedType]]]),
        parameters,
        expressionTrees(1)
      ),
      createContractTemplate(
        SType.EmptySeq,
        None,
        Parameter.EmptySeq,
        expressionTrees(2)
      )
    )
    val templateValues = Seq(
      Map("p1" -> ByteConstant(10.toByte), "p2" -> ByteConstant(40.toByte), "p3" -> ByteConstant(50.toByte)),
      Map("p1" -> IntConstant(10), "p2" -> IntConstant(20)),
      Map[String, Constant[SType]](),
    )
    val expectedErgoTree = Seq(
      ErgoTree(
        ErgoTree.ConstantSegregationHeader,
        IndexedSeq(
          ByteConstant(10.toByte),
          ByteConstant(40.toByte),
          ByteConstant(50.toByte)
        ),
        expressionTrees(0)
      ),
      ErgoTree(
        ErgoTree.ConstantSegregationHeader,
        IndexedSeq(
          IntConstant(10),
          IntConstant(20),
          IntConstant(30)
        ),
        expressionTrees(1)
      ),
      ErgoTree(
        ErgoTree.ConstantSegregationHeader,
        Constant.EmptySeq,
        expressionTrees(2)
      )
    )

    templates.indices.foreach(i =>
      templates(i).applyTemplate(templateValues(i)) shouldEqual expectedErgoTree(i)
    )
  }

  property("(de)serialization round trip") {
    val parameters = IndexedSeq(
      createParameter("p1", 0),
      createParameter("p2", 1),
      createParameter("p3", 2))
    val templates = Seq(
      createContractTemplate(
        IndexedSeq(SType.typeByte, SType.typeByte, SType.typeByte).asInstanceOf[IndexedSeq[SType]],
        Some(IndexedSeq(Some(10.toByte), Some(20.toByte), Some(30.toByte)).asInstanceOf[IndexedSeq[Option[SType#WrappedType]]]),
        parameters,
        EQ(Plus(ConstantPlaceholder(0, SType.typeByte),
                ConstantPlaceholder(1, SType.typeByte)),
           ConstantPlaceholder(2, SType.typeByte)).toSigmaProp
      ),
      createContractTemplate(
        IndexedSeq(SType.typeByte, SType.typeByte, SType.typeByte).asInstanceOf[IndexedSeq[SType]],
        Some(IndexedSeq(Some(10.toByte), None, Some(30.toByte)).asInstanceOf[IndexedSeq[Option[SType#WrappedType]]]),
        parameters,
        EQ(Plus(ConstantPlaceholder(0, SType.typeByte),
          ConstantPlaceholder(1, SType.typeByte)),
          ConstantPlaceholder(2, SType.typeByte)).toSigmaProp
      ),
      createContractTemplate(
        IndexedSeq(SType.typeShort, SType.typeShort, SType.typeShort).asInstanceOf[IndexedSeq[SType]],
        Some(IndexedSeq(Some(10.toShort), Some(20.toShort), Some(30.toShort)).asInstanceOf[IndexedSeq[Option[SType#WrappedType]]]),
        parameters,
        EQ(Plus(ConstantPlaceholder(0, SType.typeShort),
          ConstantPlaceholder(1, SType.typeShort)),
          ConstantPlaceholder(2, SType.typeShort)).toSigmaProp
      ),
      createContractTemplate(
        IndexedSeq(SType.typeInt, SType.typeInt, SType.typeInt).asInstanceOf[IndexedSeq[SType]],
        Some(IndexedSeq(Some(10), Some(20), Some(30)).asInstanceOf[IndexedSeq[Option[SType#WrappedType]]]),
        parameters,
        EQ(Plus(ConstantPlaceholder(0, SType.typeInt),
          ConstantPlaceholder(1, SType.typeInt)),
          ConstantPlaceholder(2, SType.typeInt)).toSigmaProp
      ),
      createContractTemplate(
        SType.EmptySeq,
        None,
        Parameter.EmptySeq,
        EQ(Plus(CBigInt(BigInteger.valueOf(10L)), BigIntConstant(20L)), BigIntConstant(30L)).toSigmaProp
      )
    )

    templates.foreach { template =>
      serializationRoundTrip(template)
    }
  }

  property("Data Json serialization round trip") {
    val parameters = IndexedSeq(
      createParameter("p1", 0),
      createParameter("p2", 1),
      createParameter("p3", 2))
    val templates = Seq(
      createContractTemplate(
        IndexedSeq(SType.typeByte, SType.typeByte, SType.typeByte).asInstanceOf[IndexedSeq[SType]],
        Some(IndexedSeq(Some(10.toByte), Some(20.toByte), Some(30.toByte)).asInstanceOf[IndexedSeq[Option[SType#WrappedType]]]),
        parameters,
        EQ(Plus(ConstantPlaceholder(0, SType.typeByte),
          ConstantPlaceholder(1, SType.typeByte)),
          ConstantPlaceholder(2, SType.typeByte)).toSigmaProp
      ),
      createContractTemplate(
        IndexedSeq(SType.typeByte, SType.typeByte, SType.typeByte).asInstanceOf[IndexedSeq[SType]],
        Some(IndexedSeq(Some(10.toByte), None, Some(30.toByte)).asInstanceOf[IndexedSeq[Option[SType#WrappedType]]]),
        parameters,
        EQ(Plus(ConstantPlaceholder(0, SType.typeByte),
          ConstantPlaceholder(1, SType.typeByte)),
          ConstantPlaceholder(2, SType.typeByte)).toSigmaProp
      ),
      createContractTemplate(
        IndexedSeq(SType.typeShort, SType.typeShort, SType.typeShort).asInstanceOf[IndexedSeq[SType]],
        Some(IndexedSeq(Some(10.toShort), Some(20.toShort), Some(30.toShort)).asInstanceOf[IndexedSeq[Option[SType#WrappedType]]]),
        parameters,
        EQ(Plus(ConstantPlaceholder(0, SType.typeShort),
          ConstantPlaceholder(1, SType.typeShort)),
          ConstantPlaceholder(2, SType.typeShort)).toSigmaProp
      ),
      createContractTemplate(
        IndexedSeq(SType.typeInt, SType.typeInt, SType.typeInt).asInstanceOf[IndexedSeq[SType]],
        Some(IndexedSeq(Some(10), Some(20), Some(30)).asInstanceOf[IndexedSeq[Option[SType#WrappedType]]]),
        parameters,
        EQ(Plus(ConstantPlaceholder(0, SType.typeInt),
          ConstantPlaceholder(1, SType.typeInt)),
          ConstantPlaceholder(2, SType.typeInt)).toSigmaProp
      ),
      createContractTemplate(
        SType.EmptySeq,
        None,
        Parameter.EmptySeq,
        EQ(Plus(CBigInt(BigInteger.valueOf(10L)), BigIntConstant(20L)), BigIntConstant(30L)).toSigmaProp
      )
    )

    templates.foreach { template =>
      jsonRoundtrip(template)
    }
  }
}