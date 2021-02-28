package sigmastate.interpreter

import sigmastate.serialization.ErgoTreeSerializer
import org.ergoplatform.settings.ErgoAlgos
import sigmastate.Values.ErgoTree
import sigmastate.helpers.SigmaPPrint
import special.sigma.SigmaDslTesting

class PrecompiledScriptProcessorSpecification extends SigmaDslTesting {

  def parseTree(hex: String): ErgoTree = {
    val bytes = ErgoAlgos.decodeUnsafe(hex)
    val tree = ErgoTreeSerializer.DefaultSerializer.deserializeErgoTree(bytes)
    tree
  }

  property("deserialize from hex") {
    predefScriptHexes.foreach { hex =>
      val tree = parseTree(hex)
      println(
        s"""Tree: '$hex'
          |------------------------------------------
          |""".stripMargin)
      SigmaPPrint.pprintln(tree, width = 100, height = 150)
      println()
    }
  }

  property("equality") {
    val predefTrees = predefScriptHexes.map { h => parseTree(h) }
    val extraTrees = Seq(TrueTree, FalseTree)
    val trees = extraTrees ++ predefTrees
    val scripts = trees.map { t => t.bytes: Seq[Byte] }
    val processor = PrecompiledScriptProcessor(scripts)
    trees.foreach { t =>
      processor.getVerifier(t).isDefined shouldBe true
    }
  }

  property("rejects duplicates") {
    val trees = Seq(TrueTree, TrueTree)
    val scripts = trees.map { t => t.bytes: Seq[Byte] }
    assertExceptionThrown(
      PrecompiledScriptProcessor(scripts),
      { case _: IllegalArgumentException => true
        case _ => false }
    )
  }
}
