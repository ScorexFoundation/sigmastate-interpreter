package sigmastate.helpers

import org.ergoplatform._
import org.ergoplatform.settings.ErgoAlgos
import org.ergoplatform.validation.{ValidationRules, SigmaValidationSettings}
import sigmastate.eval.{IRContext, CompiletimeIRContext}
import sigmastate.interpreter.Interpreter.{ScriptNameProp, emptyEnv}
import sigmastate.interpreter.{CacheKey, PrecompiledScriptProcessor, ProcessorStats, ScriptProcessorSettings}

import scala.util.{Success, Failure}

/** Base class for interpreters used in tests.
  * @see derived classes */
class ErgoLikeTestInterpreter(implicit override val IR: IRContext) extends ErgoLikeInterpreter {
  override type CTX = ErgoLikeContext
  override val precompiledScriptProcessor = ErgoLikeTestInterpreter.DefaultProcessorInTests
}
object ErgoLikeTestInterpreter {
  val predefScriptsInTests = Seq(
    // for property("emission specification")
    "100b0400040004e04e0580bcc1960b0580bcc1960b05808c8d9e02040204e04e04a00b040005808c8d9e02d1ec9683040193e4c6b2a57300000404a391a3e4c6a7040493c2a7c2b2a573010093958fa3730273039973049c73057e9a73069d99a3730773080599c1a7c1b2a5730900ed91a3e4c6a7040490c1a7730a"
  )

  /** A list of settings, one for each soft-fork. */
  val validationSettings: Seq[SigmaValidationSettings] = Array(
    ValidationRules.currentSettings // for v4.0
    // add v5.0 settings here...
  )

  /** Script processor which uses [[CompiletimeIRContext]] to process graphs. */
  val DefaultProcessorInTests: PrecompiledScriptProcessor = {
    val scriptKeys = predefScriptsInTests.flatMap { s =>
      val bytes = ErgoAlgos.decodeUnsafe(s)
      validationSettings.map(vs => CacheKey(bytes, vs))
    }

    new PrecompiledScriptProcessor(
      ScriptProcessorSettings(
        predefScripts = scriptKeys,
        maxCacheSize = 10,
        recordCacheStats = true,
        reportingInterval = 10)) {
      override protected def createIR(): IRContext = new CompiletimeIRContext

      override protected def onReportStats(stats: ProcessorStats): Unit = {
        println(s"Processor Stats: $stats")
      }

      override protected def onEvictedCacheEntry(key: CacheKey): Unit = {
        val scriptHex = ErgoAlgos.encode(key.ergoTreeBytes.toArray)
        println(s"Evicted: ${scriptHex}")
      }
    }
  }
}

class ErgoTransactionValidator(activatedVersion: Byte)(implicit IR: IRContext) {
  val verifier = new ErgoLikeTestInterpreter()

  def validate(tx: ErgoLikeTransaction,
               blockchainState: BlockchainState,
               minerPubkey: Array[Byte],
               boxesReader: ErgoBoxReader): Either[Throwable, Long] = {

    val msg = tx.messageToSign
    val inputs = tx.inputs

    val boxes: IndexedSeq[ErgoBox] = tx.inputs.map(_.boxId).map{id =>
      boxesReader.byId(id) match {
        case Success(box) => box
        case Failure(e) => return Left[Throwable, Long](e)
      }
    }

    val txCost = boxes.zipWithIndex.foldLeft(0L) { case (accCost, (box, idx)) =>
      val input = inputs(idx)
      val proof = input.spendingProof

      val proverExtension = tx.inputs(idx).spendingProof.extension

      val context =
        ErgoLikeContextTesting(blockchainState.currentHeight, blockchainState.lastBlockUtxoRoot, minerPubkey, boxes,
          tx, box, activatedVersion, proverExtension)
      val verificationResult = verifier.verify(
        emptyEnv + (ScriptNameProp -> s"height_${blockchainState.currentHeight }_verify"),
        box.ergoTree, context, proof, msg)
      val scriptCost: Long = verificationResult match {
        case Success((res, cost)) =>
          if(!res) return Left[Throwable, Long](new Exception(s"Validation failed for input #$idx"))
          else cost
        case Failure(e) =>
          return Left[Throwable, Long](e)
      }
      accCost + scriptCost
    }
    Right(txCost)
  }
}