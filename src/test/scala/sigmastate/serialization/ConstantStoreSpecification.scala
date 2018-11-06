package sigmastate.serialization

import org.ergoplatform.Self
import sigmastate.Values.{BlockValue, Constant, ConstantPlaceholder, IntConstant, LongConstant, ValDef, ValUse, Value}
import sigmastate._
import sigmastate.helpers.SigmaTestingCommons
import sigmastate.lang.{DeserializationSigmaBuilder, SigmaBuilder}
import sigmastate.utxo.ExtractAmount

class ConstantStoreSpecification extends SerializationSpecification with SigmaTestingCommons {

  implicit lazy val IR: TestingIRContext = new TestingIRContext
  implicit val builder: SigmaBuilder = DeserializationSigmaBuilder

  property("extract constants") {
    val script = Plus(10, 20)
    val extractedConstants = Seq(IntConstant(10), IntConstant(20))
    val scriptWithPlaceholders = Plus(ConstantPlaceholder(0, SInt), ConstantPlaceholder(1, SInt))
    ConstantStore.extractConstants(script) shouldEqual (extractedConstants, scriptWithPlaceholders)
  }

  property("no constants to extract") {
    val script = Plus(ExtractAmount(Self), ExtractAmount(Self))
    val extractedConstants = Seq()
    val scriptWithPlaceholders = BlockValue(Vector(ValDef(1,List(),ExtractAmount(Self))),Plus(ValUse(1,SLong),ValUse(1,SLong)))
    ConstantStore.extractConstants(script) shouldEqual (extractedConstants, scriptWithPlaceholders)
  }

  property("empty store should have no constants") {
    val s = new ConstantStore()
    s.getAll shouldBe empty
  }

  property("put/get constant") {
    val s = new ConstantStore()
    val c = IntConstant(1)
    val ph = s.put(c)
    ph.tpe shouldEqual c.tpe
    s.get(ph.id) shouldEqual c
    s.getAll.size shouldBe 1
    s.getAll.head shouldEqual c
  }

  property("same constant put should produce a new placeholder(index)") {
    // since the same constant in different places of the tree might have different semantics
    val s = new ConstantStore()
    val c = IntConstant(1)
    val ph1 = s.put(c)
    val ph2 = s.put(c)
    ph1 should not equal ph2
    s.getAll.size shouldBe 2
    s.get(ph1.id) shouldEqual c
    s.get(ph2.id) shouldEqual c
  }

  property("pass constants on instantiation, should be accessible by index") {
    val c1 = IntConstant(1).asInstanceOf[Constant[SType]]
    val c2 = IntConstant(2).asInstanceOf[Constant[SType]]
    val constants = IndexedSeq(c1, c2)
    val s = new ConstantStore(constants)
    s.getAll.size shouldBe 2
    s.get(0) shouldEqual c1
    s.get(1) shouldEqual c2
  }

}
