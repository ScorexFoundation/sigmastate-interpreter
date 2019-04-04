package sigmastate.utxo.examples

import sigmastate.eval.{CompiletimeCosting, IRContext}
import sigmastate.helpers.SigmaTestingCommons
import sigmastate.interpreter.Interpreter.ScriptNameProp

class LetsSpecification extends SigmaTestingCommons { suite =>
  // Not mixed with TestContext since it is not possible to call compiler.compile outside tests if mixed
  implicit lazy val IR: IRContext = new IRContext with CompiletimeCosting

  val env = Map(
    ScriptNameProp -> "withdrawalScriptEnv"
  )

  val exchangeScript = compiler.compile(env,
    """{
      |  val lookupProof = getVar[Coll[Byte]](2).get
      |
      |  val treeHolderBox = CONTEXT.dataInputs(0)
      |  val membersTree = treeHolderBox.R4[AvlTree].get
      |
      |  val participant0 = INPUTS(0)
      |  val participant1 = INPUTS(1)
      |
      |  val participantOut0 = OUTPUTS(0)
      |  val participantOut1 = OUTPUTS(1)
      |
      |  val token0 = participant0.tokens(0)._1
      |  val token1 = participant1.tokens(1)._1
      |
      |  val memberTokens = Coll(token0, token1)
      |
      |  val membersExist = membersTree.getMany(memberTokens, lookupProof).forall({ (o: Option[Coll[Byte]]) => o.isDefined })
      |
      |  val initialBalance0 = participant0.R4[Long].get
      |
      |  val initialBalance1 = participant1.R4[Long].get
      |
      |  val finishBalance0  = participantOut0.R4[Long].get
      |
      |  val finishBalance1  = participantOut1.R4[Long].get
      |
      |  val script0Saved = participantOut0.propositionBytes == participant0.propositionBytes
      |  val script1Saved = participantOut1.propositionBytes == participant1.propositionBytes
      |  val scriptsSaved = script0Saved && script1Saved
      |
      |  val diff0 = finishBalance0 - initialBalance0
      |  val diff1 = finishBalance1 - finishBalance1
      |
      |  val diffCorrect = diff0 == -diff1
      |
      |  val selfPubKey = SELF.R5[SigmaProp].get
      |
      |  selfPubKey && membersExist && diffCorrect && scriptsSaved
      |
      |}""".stripMargin
  )

  println(exchangeScript)

}
