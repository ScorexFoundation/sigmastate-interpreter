package sigmastate.lang

import org.ergoplatform.Outputs
import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, PropSpec}
import sigmastate.Values._
import sigmastate._
import sigmastate.lang.Terms.Ident
import sigmastate.serialization.generators.{ConcreteCollectionGenerators, TransformerGenerators, ValueGenerators}
import sigmastate.utxo._

class SigmaSpecializerTest extends PropSpec
  with PropertyChecks
  with Matchers
  with LangTests
  with ValueGenerators
  with ConcreteCollectionGenerators
  with TransformerGenerators {

  private def countANDORInputNodes(root: Transformer[SCollection[SBoolean.type], SBoolean.type]): Int =
    root.input.items.foldLeft(0) { (sum, item) =>
      item match {
        case r@AND(_) => sum + countANDORInputNodes(r)
        case r@OR(_) => sum + countANDORInputNodes(r)
        case _ => sum + 1
      }
    }

  def typed(env: Map[String, SValue], x: String): SValue = {
    val parsed = SigmaParser(x).get.value
    val binder = new SigmaBinder(env)
    val bound = binder.bind(parsed)
    val typer = new SigmaTyper
    val typed = typer.typecheck(bound)
    typed
  }
  def spec(env: Map[String, SValue], typed: SValue): SValue = {
    val spec = new SigmaSpecializer()
    spec.specialize(env, typed)
  }
  def spec(code: String): SValue = {
    spec(Map(), typed(Map(), code))
  }
  def fail(env: Map[String, SValue], code: String, messageSubstr: String = ""): Unit = {
    try {
      spec(env, typed(env, code))
      assert(false, s"Should fail: $code")
    } catch {
      case e: SpecializerException =>
        if (messageSubstr.nonEmpty)
          if (!e.getMessage.contains(messageSubstr)) {
            throw new AssertionError(s"Error message '${e.getMessage}' doesn't contain '$messageSubstr'.", e)
          }
    }
  }

  property("resolve let-bound names and substitute") {
    spec(Map("X" -> LongConstant(10)),
         Ident("X", SLong)) shouldBe LongConstant(10)
    spec(Map("X" -> LongConstant(10)),
         Plus(Ident("X", SLong).asValue[SLong.type], LongConstant(1))) shouldBe Plus(10L, 1L)
  }

  property("substitute all let expressions in block result") {
    spec("{ let X = 10; X }") shouldBe IntConstant(10)
    spec("{ let X = 10; let Y = 20; X + Y }") shouldBe Plus(10, 20)
    spec("{ let X = 10; let Y = 20; X + Y + X }") shouldBe Plus(Plus(10, 20), 10)
    spec("{ let X = 10 + 1; X + X}") shouldBe Plus(Plus(10, 1), Plus(10, 1))
    spec("{ let X = 10; let Y = X; Y}") shouldBe IntConstant(10)
    spec("{ let X = 10; let Y = X; let Z = Y; Z }") shouldBe IntConstant(10)
    spec("{ let X = 10; let Y = X + 1; let Z = Y + X; Z + Y + X }") shouldBe
      Plus(Plus(/*Z=*/Plus(/*Y=*/Plus(10, 1), 10), /*Y=*/Plus(10, 1)), 10)
  }

  property("Option constructors") {
    fail(Map(), "None", "Option values are not supported")
    fail(Map(), "Some(10)", "Option values are not supported")
  }

  property("generic methods of arrays") {
    spec("OUTPUTS.map(fun (out: Box) = { out.value >= 10 })") shouldBe
      MapCollection(Outputs, 21, GE(ExtractAmount(TaggedBox(21)), LongConstant(10)))
    spec("OUTPUTS.exists(fun (out: Box) = { out.value >= 10 })") shouldBe
        Exists(Outputs, 21, GE(ExtractAmount(TaggedBox(21)), LongConstant(10)))
    spec("OUTPUTS.forall(fun (out: Box) = { out.value >= 10 })") shouldBe
        ForAll(Outputs, 21, GE(ExtractAmount(TaggedBox(21)), LongConstant(10)))
    spec("{ let arr = Array(1,2); arr.fold(0, fun (n1: Int, n2: Int) = n1 + n2)}") shouldBe
        Fold(ConcreteCollection(IntConstant(1), IntConstant(2)),
             21, IntConstant(0), 22, Plus(TaggedInt(21), TaggedInt(22)))
    spec("OUTPUTS.slice(0, 10)") shouldBe
        Slice(Outputs, IntConstant(0), IntConstant(10))
    spec("OUTPUTS.where(fun (out: Box) = { out.value >= 10 })") shouldBe
        Where(Outputs, 21, GE(ExtractAmount(TaggedBox(21)), LongConstant(10)))
  }

  property("AND flattening predefined") {
    spec("true && true") shouldBe AND(TrueLeaf, TrueLeaf)
    spec("true && false") shouldBe AND(TrueLeaf, FalseLeaf)
    spec("true && (true && 10 == 10)") shouldBe
      AND(TrueLeaf, TrueLeaf, EQ(IntConstant(10), IntConstant(10)))
    spec("true && true && true") shouldBe AND(TrueLeaf, TrueLeaf, TrueLeaf)
    spec("true && (true && (true && true)) && true") shouldBe
      AND(TrueLeaf, TrueLeaf, TrueLeaf, TrueLeaf, TrueLeaf)
  }

  property("AND flattening, CAND/COR untouched") {
    val sigmaBooleans1 = AND(Seq(TrueLeaf, CAND(Seq(proveDlogGen.sample.get, proveDHTGen.sample.get))))
    spec(Map(), sigmaBooleans1) shouldBe sigmaBooleans1
    val sigmaBooleans2 = AND(Seq(TrueLeaf, COR(Seq(proveDlogGen.sample.get, proveDHTGen.sample.get))))
    spec(Map(), sigmaBooleans2) shouldBe sigmaBooleans2
  }

  property("AND flattening") {
    forAll(logicalExprTreeNodeGen(Seq(AND.apply))) { tree =>
      val out = spec(Map(), tree).asInstanceOf[AND]
      out.input.items.forall(!_.isInstanceOf[AND]) shouldBe true
      countANDORInputNodes(tree) shouldBe out.input.items.length
    }
  }

  property("OR flattening predefined") {
    spec("true || true || true") shouldBe OR(TrueLeaf, TrueLeaf, TrueLeaf)
    spec("true || (true || true) || true") shouldBe
      OR(TrueLeaf, TrueLeaf, TrueLeaf, TrueLeaf)
  }

  property("OR flattening, CAND/COR untouched") {
    val sigmaBooleans1 = OR(Seq(TrueLeaf, CAND(Seq(proveDlogGen.sample.get, proveDHTGen.sample.get))))
    spec(Map(), sigmaBooleans1) shouldBe sigmaBooleans1
    val sigmaBooleans2 = OR(Seq(TrueLeaf, COR(Seq(proveDlogGen.sample.get, proveDHTGen.sample.get))))
    spec(Map(), sigmaBooleans2) shouldBe sigmaBooleans2
  }

  property("OR flattening") {
    forAll(logicalExprTreeNodeGen(Seq(OR.apply))) { tree =>
      val out = spec(Map(), tree).asInstanceOf[OR]
      out.input.items.forall(!_.isInstanceOf[OR]) shouldBe true
      countANDORInputNodes(tree) shouldBe out.input.items.length
    }
  }
}
