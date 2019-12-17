package sigmastate.verification.test

import org.ergoplatform.ErgoBox.{R2, R4}
import org.ergoplatform.{ErgoBox, Height, Outputs, Self}
import org.scalacheck.Arbitrary.arbLong
import sigmastate.Values.{BlockValue, ByteArrayConstant, ConstantNode, IntConstant, LongArrayConstant, LongConstant, SigmaPropConstant, ValDef, ValUse, Value}
import sigmastate.eval.CSigmaProp
import sigmastate.helpers.{ContextEnrichingTestProvingInterpreter, ErgoLikeContextTesting, ErgoLikeTestInterpreter, SigmaTestingCommons}
import sigmastate.utxo.{ByIndex, ExtractId, ExtractRegisterAs, ExtractScriptBytes, OptionIsDefined, SelectField, SigmaPropBytes, SizeOf}
import sigmastate.verification.contract.{AssetsAtomicExchangeCompilation, DummyContractCompilation}
import sigmastate.verified.VerifiedTypeConverters._
import sigmastate.{utxo, _}
import sigmastate.basics.DLogProtocol.ProveDlog
import special.collection.{Coll, CollOverArray}
import special.sigma.SigmaProp

class AssetsAtomicExchangeCompilationTest extends SigmaTestingCommons with MiscGenerators {

  implicit lazy val IR: TestingIRContext = new TestingIRContext

  property("buyer contract ergo tree") {
    forAll(unsignedIntGen, byteCollGen(0, 40), arbLong.arbitrary, proveDlogGen) {
      case (deadline, tokenId, tokenAmount, proveDlogPk) =>
        val pk: SigmaProp = CSigmaProp(proveDlogPk)
        val c = AssetsAtomicExchangeCompilation.buyerContractInstance(deadline, tokenId, tokenAmount, pk)
        val expectedProp = SigmaOr(Seq(
          SigmaAnd(List(
            BoolToSigmaProp(GT(Height,IntConstant(0))),
            SigmaPropConstant(proveDlogPk))
          ),
          BinAnd(
            BinAnd(
              GT(
                SizeOf(Outputs),
                IntConstant(0)
              ),
              OptionIsDefined(
                ExtractRegisterAs(ByIndex(Outputs,IntConstant(0),None),R4,SOption(SCollectionType(SByte)))
              )
            ),
            BlockValue(
              Vector(
                // tokens
                ValDef(1,List(),
                  ExtractRegisterAs(
                    ByIndex(Outputs,IntConstant(0),None),
                    R2,
                    SOption( SCollectionType(STuple(SCollectionType(SByte),SLong)))
                  ).get
                ),
                ValDef(2,List(),
                  BinAnd(
                    BinAnd(
                      GT(
                        SizeOf(ValUse(1, SCollectionType(STuple(SCollectionType(SByte), SLong)))),
                        IntConstant(0)
                      ),
                      EQ(
                        SelectField(
                          ByIndex(
                            ValUse(1, SCollectionType(STuple(SCollectionType(SByte), SLong))),
                            IntConstant(0),None
                          ),
                          1
                        ),
                        ByteArrayConstant(tokenId)
                      )
                    ),
                    GE(SelectField(ByIndex(ValUse(1, SCollectionType(STuple(SCollectionType(SByte), SLong))),IntConstant(0),None),2),
                      LongConstant(tokenAmount)
                    )
                  )
                ),
                ValDef(3,List(),EQ(
                  ExtractRegisterAs(ByIndex(Outputs,IntConstant(0),None),R4, SOption(SCollectionType(SByte))).get,
                  ExtractId(Self)))
              ),
              BinAnd(
                BinAnd(
                  ValUse(2,SBoolean),
                  EQ(
                    ExtractScriptBytes(ByIndex(Outputs,IntConstant(0),None)),
                    SigmaPropBytes(SigmaPropConstant(proveDlogPk))
                  )
                ),
                ValUse(3,SBoolean))).asInstanceOf[Value[SBoolean.type]]
          ).toSigmaProp
        ))
        assert(c.prop == expectedProp)
    }
  }

  ignore("buyer contract ergo tree test") {
    val prover = new ContextEnrichingTestProvingInterpreter
    val verifier = new ErgoLikeTestInterpreter

    val pubkey = prover.dlogSecrets.head.publicImage

    val pk: SigmaProp = CSigmaProp(pubkey)
    val c = AssetsAtomicExchangeCompilation.buyerContractInstance(50, byteCollGen(20).sample.get, 1, pk)
    val tree = c.ergoTree

    val spendingTransaction = createTransaction(IndexedSeq(ErgoBox(1, tree, 0)))

    val ctx = ErgoLikeContextTesting(
      currentHeight = 50,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      minerPubkey = ErgoLikeContextTesting.dummyPubkey,
      boxesToSpend = IndexedSeq(fakeSelf),
      spendingTransaction,
      self = fakeSelf)

    val pr = prover.prove(tree, ctx, fakeMessage).get
    verifier.verify(tree, ctx, pr, fakeMessage).get._1 shouldBe true
  }

}
