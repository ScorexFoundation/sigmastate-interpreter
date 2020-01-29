package sigmastate.interpreter

import sigmastate.interpreter.Interpreter.ScriptEnv
import org.scalatest.{PropSpec, Matchers}
import org.scalatest.prop.PropertyChecks

import scala.util.{Success, Failure, Try}
import org.ergoplatform.dsl.{SigmaContractSyntax, TestContractSpec}
import sigmastate.utxo.ComplexityTableStat
import special.collection.Coll
import special.sigma.{SigmaTestingData, SigmaTypeGens, BigInt}
import sigmastate.eval.Extensions._
import sigma.util.Extensions._
import scalan.util.Extensions._

class ErgoTreeEvaluatorSpec extends PropSpec
    with PropertyChecks
    with Matchers
    with SigmaTestingData with SigmaContractSyntax
    with SigmaTypeGens { suite =>

  lazy val spec = TestContractSpec(suite)(new TestingIRContext)

  override def contractEnv: ScriptEnv = Map()

  implicit lazy val IR = new TestingIRContext {
    override val okPrintEvaluatedEntries: Boolean = false
    override val okMeasureOperationTime: Boolean = true
  }

  property("Byte methods equivalence") {
    val toByte = checkEq(funcNew[Byte, Byte]("{ (x: Byte) => x.toByte }"))(x => x.toByte)
    val toShort = checkEq(func[Byte,Short]("{ (x: Byte) => x.toShort }"))(x => x.toShort)
    val toInt = checkEq(func[Byte,Int]("{ (x: Byte) => x.toInt }"))(x => x.toInt)
    val toLong = checkEq(func[Byte,Long]("{ (x: Byte) => x.toLong }"))(x => x.toLong)
    val toBigInt = checkEq(func[Byte,BigInt]("{ (x: Byte) => x.toBigInt }"))(x => x.toBigInt)

    //TODO soft-fork: for new 4 operations below
    lazy val toBytes = checkEq(func[Byte,Coll[Byte]]("{ (x: Byte) => x.toBytes }"))(x => x.toBytes)
    lazy val toBits = checkEq(func[Byte,Coll[Boolean]]("{ (x: Byte) => x.toBits }"))(x => x.toBits)
    lazy val toAbs = checkEq(func[Byte,Byte]("{ (x: Byte) => x.toAbs }"))(x => x.toAbs)
    lazy val compareTo = checkEq(func[(Byte, Byte), Int]("{ (x: (Byte, Byte)) => x._1.compareTo(x._2) }"))({ (x: (Byte, Byte)) => x._1.compareTo(x._2) })

    forAll { x: Byte =>
      Seq(toByte, toShort, toInt, toLong, toBigInt).foreach(_(x))
    }
//    println(ComplexityTableStat.complexityTableString)
  }

}
