package sigmastate

import org.ergoplatform._
import scorex.util.ModifierId
import sigmastate.Values._
import sigmastate.basics.ProveDHTuple
import sigmastate.eval._
import sigmastate.utxo._
import special.collection._
import sigmastate.utils.Helpers
import special.sigma.{SigmaDslTesting, Box}

import scala.util.Success

/** Specification to verify that the interpreter behaves according to docs/aot-jit-switch.md. */
class ScriptVersionSwitchSpecification extends SigmaDslTesting {
  override implicit val generatorDrivenConfig = PropertyCheckConfiguration(minSuccessful = 30)
  implicit def IR = createIR()

  /** Rule#| SF Status| Block Type| Script Version | Release | Validation Action
    * -----|----------|-----------|----------------|---------|--------
    * 1    | inactive | candidate | Script v1      | v4.0    | R4.0-AOT-cost, R4.0-AOT-verify
    */
  property("Rule 1 | inactive SF | candidate block | Script v1") {
    val samples = genSamples[Coll[Box]](collOfN[Box](5), MinSuccessful(20))
    val b1 = CostingBox(
      false,
      new ErgoBox(
        1L,
        new ErgoTree(
          0.toByte,
          Vector(),
          Right(
            SigmaPropConstant(
              CSigmaProp(
                ProveDHTuple(
                  Helpers.decodeECPoint("02c1a9311ecf1e76c787ba4b1c0e10157b4f6d1e4db3ef0d84f411c99f2d4d2c5b"),
                  Helpers.decodeECPoint("027d1bd9a437e73726ceddecc162e5c85f79aee4798505bc826b8ad1813148e419"),
                  Helpers.decodeECPoint("0257cff6d06fe15d1004596eeb97a7f67755188501e36adc49bd807fe65e9d8281"),
                  Helpers.decodeECPoint("033c6021cff6ba5fdfc4f1742486030d2ebbffd9c9c09e488792f3102b2dcdabd5")
                )
              )
            )
          )
        ),
        Coll(),
        Map(
          ErgoBox.R4 -> ByteArrayConstant(
            Helpers.decodeBytes(
              "7200004cccdac3008001bc80ffc7ff9633bca3e501801380ff007900019d7f0001a8c9dfff5600d964011617ca00583f989c7f80007fee7f99b07f7f870067dc315180828080307fbdf400"
            )
          ),
          ErgoBox.R7 -> LongConstant(0L),
          ErgoBox.R6 -> FalseLeaf,
          ErgoBox.R5 -> ByteArrayConstant(Helpers.decodeBytes("7f"))
        ),
        ModifierId @@ ("7dffff48ab0000c101a2eac9ff17017f6180aa7fc6f2178000800179499380a5"),
        21591.toShort,
        638768
      )
    )
    val b2 = CostingBox(
      false,
      new ErgoBox(
        1000000000L,
        new ErgoTree(
          0.toByte,
          Vector(),
          Right(BoolToSigmaProp(OR(ConcreteCollection(Array(FalseLeaf, AND(ConcreteCollection(Array(FalseLeaf, FalseLeaf), SBoolean))), SBoolean))))
        ),
        Coll(),
        Map(),
        ModifierId @@ ("008677ffff7ff36dff00f68031140400007689ff014c9201ce8000a9ffe6ceff"),
        32767.toShort,
        32827
      )
    )

    verifyCases(
      {
        def success[T](v: T, c: Int) = Success(Expected(v, c))
        Seq(
          (Coll[Box](), success(Coll[Box](), 37297)),
          (Coll[Box](b1), success(Coll[Box](), 37397)),
          (Coll[Box](b1, b2), success(Coll[Box](b2), 37537))
        )
      },
      existingFeature({ (x: Coll[Box]) => x.filter({ (b: Box) => b.value > 1 }) },
      "{ (x: Coll[Box]) => x.filter({(b: Box) => b.value > 1 }) }",
      FuncValue(
        Vector((1, SCollectionType(SBox))),
        Filter(
          ValUse(1, SCollectionType(SBox)),
          FuncValue(Vector((3, SBox)), GT(ExtractAmount(ValUse(3, SBox)), LongConstant(1L)))
        )
      )),
      preGeneratedSamples = Some(samples))

  }
}
