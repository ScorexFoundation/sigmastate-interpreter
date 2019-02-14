package special.sigma {
  import scalan._

  trait SigmaDslCosted extends Base { self: SigmaLibrary =>
    import AnyValue._;
    import AvlTree._;
    import Box._;
    import CCostedAvlTree._;
    import CCostedBox._;
    import CCostedColl._;
    import CCostedContext._;
    import CCostedPrim._;
    import Coll._;
    import CollBuilder._;
    import Context._;
    import CostModel._;
    import Costed._;
    import CostedAvlTree._;
    import CostedBox._;
    import CostedColl._;
    import CostedContext._;
    import CostedOption._;
    import SigmaDslBuilder._;
    import TestSigmaDslBuilder._;
    import WOption._;  // manual fix
    import WSpecialPredef._; // manual fix

    abstract class CCostedContext(val ctx: Rep[Context]) extends CostedContext {
      def dsl: Rep[SigmaDslBuilder] = RTestSigmaDslBuilder();
      def OUTPUTS: Rep[CostedColl[Box]] = CCostedContext.this.dsl.costBoxes(CCostedContext.this.ctx.OUTPUTS);
      def INPUTS: Rep[CostedColl[Box]] = CCostedContext.this.dsl.costBoxes(CCostedContext.this.ctx.INPUTS);
      def HEIGHT: Rep[Costed[Int]] = {
        val cost: Rep[Int] = CCostedContext.this.dsl.CostModel.SelectField;
        RCCostedPrim(CCostedContext.this.ctx.HEIGHT, cost, toRep(4L.asInstanceOf[Long]))
      };
      def SELF: Rep[CostedBox] = RCCostedBox(CCostedContext.this.ctx.SELF, CCostedContext.this.dsl.CostModel.AccessBox);
      def LastBlockUtxoRootHash: Rep[CostedAvlTree] = RCCostedAvlTree(CCostedContext.this.ctx.LastBlockUtxoRootHash, CCostedContext.this.dsl.CostModel.AccessAvlTree);
      def MinerPubKey: Rep[CostedColl[Byte]] = CCostedContext.this.dsl.costColWithConstSizedItem[Byte](CCostedContext.this.ctx.MinerPubKey, CCostedContext.this.dsl.CostModel.PubKeySize.toInt, toRep(1L.asInstanceOf[Long]));
      def getVar[T](id: Rep[Byte])(implicit cT: Elem[T]): Rep[CostedOption[T]] = {
        val opt: Rep[WOption[T]] = CCostedContext.this.ctx.getVar[T](id);
        CCostedContext.this.dsl.costOption[T](opt, CCostedContext.this.dsl.CostModel.GetVar)
      };
      @NeverInline def getConstant[T](id: Rep[Byte])(implicit cT: Elem[T]): Rep[Costed[T]] = delayInvoke;
      def value: Rep[Context] = CCostedContext.this.ctx;
      def cost: Rep[Int] = CCostedContext.this.ctx.cost;
      def dataSize: Rep[Long] = CCostedContext.this.ctx.dataSize
    };
    abstract class CCostedBox(val box: Rep[Box], val cost: Rep[Int]) extends CostedBox {
      def dsl: Rep[SigmaDslBuilder] = RTestSigmaDslBuilder();
      def id: Rep[CostedColl[Byte]] = CCostedBox.this.dsl.costColWithConstSizedItem[Byte](CCostedBox.this.box.id, CCostedBox.this.box.id.length, toRep(1L.asInstanceOf[Long]));
      def valueCosted: Rep[Costed[Long]] = {
        val cost: Rep[Int] = CCostedBox.this.dsl.CostModel.SelectField;
        RCCostedPrim(CCostedBox.this.box.value, cost, toRep(8L.asInstanceOf[Long]))
      };
      def bytes: Rep[CostedColl[Byte]] = CCostedBox.this.dsl.costColWithConstSizedItem[Byte](CCostedBox.this.box.bytes, CCostedBox.this.box.bytes.length, toRep(1L.asInstanceOf[Long]));
      def bytesWithoutRef: Rep[CostedColl[Byte]] = CCostedBox.this.dsl.costColWithConstSizedItem[Byte](CCostedBox.this.box.bytesWithoutRef, CCostedBox.this.box.bytesWithoutRef.length, toRep(1L.asInstanceOf[Long]));
      def propositionBytes: Rep[CostedColl[Byte]] = CCostedBox.this.dsl.costColWithConstSizedItem[Byte](CCostedBox.this.box.propositionBytes, CCostedBox.this.box.propositionBytes.length, toRep(1L.asInstanceOf[Long]));
      def registers: Rep[CostedColl[AnyValue]] = {
        val len: Rep[Int] = CCostedBox.this.box.registers.length;
        val costs: Rep[Coll[Int]] = CCostedBox.this.dsl.Colls.replicate[Int](len, CCostedBox.this.dsl.CostModel.AccessBox);
        val sizes: Rep[Coll[Long]] = CCostedBox.this.box.registers.map[Long](fun(((o: Rep[AnyValue]) => o.dataSize)));
        RCCostedColl(CCostedBox.this.box.registers, costs, sizes, CCostedBox.this.dsl.CostModel.CollectionConst)
      };
      def getReg[T](id: Rep[Int])(implicit cT: Elem[T]): Rep[CostedOption[T]] = {
        val opt: Rep[WOption[T]] = CCostedBox.this.box.getReg[T](id);
        CCostedBox.this.dsl.costOption[T](opt, CCostedBox.this.dsl.CostModel.GetRegister)
      };
      @NeverInline def creationInfo: Rep[Costed[scala.Tuple2[Int, Coll[Byte]]]] = delayInvoke;
      def value: Rep[Box] = CCostedBox.this.box;
      def dataSize: Rep[Long] = CCostedBox.this.box.dataSize
    };
    abstract class CCostedAvlTree(val tree: Rep[AvlTree], val cost: Rep[Int]) extends CostedAvlTree {
      def dsl: Rep[SigmaDslBuilder] = RTestSigmaDslBuilder();
      def startingDigest: Rep[CostedColl[Byte]] = CCostedAvlTree.this.dsl.costColWithConstSizedItem[Byte](CCostedAvlTree.this.tree.startingDigest, CCostedAvlTree.this.dsl.CostModel.PubKeySize.toInt, toRep(1L.asInstanceOf[Long]));
      def keyLength: Rep[Costed[Int]] = RCCostedPrim(CCostedAvlTree.this.tree.keyLength, CCostedAvlTree.this.dsl.CostModel.SelectField, toRep(4L.asInstanceOf[Long]));
      def valueLengthOpt: Rep[CostedOption[Int]] = CCostedAvlTree.this.dsl.costOption[Int](CCostedAvlTree.this.tree.valueLengthOpt, CCostedAvlTree.this.dsl.CostModel.SelectField);
      def maxNumOperations: Rep[CostedOption[Int]] = CCostedAvlTree.this.dsl.costOption[Int](CCostedAvlTree.this.tree.maxNumOperations, CCostedAvlTree.this.dsl.CostModel.SelectField);
      def maxDeletes: Rep[CostedOption[Int]] = CCostedAvlTree.this.dsl.costOption[Int](CCostedAvlTree.this.tree.maxDeletes, CCostedAvlTree.this.dsl.CostModel.SelectField);
      def value: Rep[AvlTree] = CCostedAvlTree.this.tree;
      def dataSize: Rep[Long] = CCostedAvlTree.this.tree.dataSize
    };
    trait CCostedContextCompanion;
    trait CCostedBoxCompanion;
    trait CCostedAvlTreeCompanion
  }
}