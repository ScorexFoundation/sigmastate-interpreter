package sigmastate.interpreter

import org.ergoplatform.validation.ValidationRules
import sigmastate.helpers.SigmaPPrint
import special.sigma.SigmaDslTesting

class PrecompiledScriptProcessorSpecification extends SigmaDslTesting {

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
    val scripts = trees.map { t => CacheKey(t.bytes, ValidationRules.currentSettings) }
    val processor = new PrecompiledScriptProcessor(ScriptProcessorSettings(scripts))
    trees.foreach { t =>
      processor.getReducer(t, ValidationRules.currentSettings) should not be null
    }
  }

  property("rejects duplicates") {
    val trees = Seq(TrueTree, TrueTree)
    val scripts = trees.map { t => CacheKey(t.bytes, ValidationRules.currentSettings) }
    assertExceptionThrown(
      new PrecompiledScriptProcessor(ScriptProcessorSettings(scripts)),
      { case _: IllegalArgumentException => true
        case _ => false }
    )
  }

  property("collects predef hit statistics") {
    val predefTrees = predefScriptHexes.map { h => parseTree(h) }
    val scriptKeys = predefTrees.map { t => CacheKey(t.bytes, ValidationRules.currentSettings) }
    Array(false, true).foreach { recordStats =>
      val processor = new PrecompiledScriptProcessor(
        ScriptProcessorSettings(
          predefScripts = scriptKeys,
          recordCacheStats = recordStats))
      predefTrees.foreach { t =>
        processor.getReducer(t, ValidationRules.currentSettings) should not be null
      }
      val expectedHits = if (recordStats) 1 else 0
      processor.getPredefStats() shouldBe Array.fill(scriptKeys.length)(expectedHits).toSeq
    }
  }
}
