package special.sigma {
  import scalan._

  trait SigmaDslCosted extends Base { self: SigmaLibrary =>
    import AnyValue._;
    import AvlTree._;
    import Box._;
    import CCostedBox._;
    import CCostedColl._;
    import CCostedContext._;
    import CCostedPrim._;
    import Coll._;
    import CollBuilder._;
    import Context._;
    import CostModel._;
    import Costed._;
    import CostedBox._;
    import CostedColl._;
    import CostedContext._;
    import CostedOption._;
    import Header._;
    import PreHeader._;
    import SigmaDslBuilder._;
    import TestSigmaDslBuilder._;
    abstract class CCostedContext(val ctx: Rep[Context]) extends CostedContext {
      def dsl: Rep[SigmaDslBuilder] = RTestSigmaDslBuilder();
      def dataInputs: Rep[CostedColl[Box]] = CCostedContext.this.dsl.costBoxes(CCostedContext.this.ctx.dataInputs);
      def OUTPUTS: Rep[CostedColl[Box]] = CCostedContext.this.dsl.costBoxes(CCostedContext.this.ctx.OUTPUTS);
      def INPUTS: Rep[CostedColl[Box]] = CCostedContext.this.dsl.costBoxes(CCostedContext.this.ctx.INPUTS);
      def HEIGHT: Rep[Costed[Int]] = {
        val cost: Rep[Int] = CCostedContext.this.dsl.CostModel.SelectField;
        RCCostedPrim(CCostedContext.this.ctx.HEIGHT, cost, toRep(4L.asInstanceOf[Long]))
      };
      def SELF: Rep[CostedBox] = RCCostedBox(CCostedContext.this.ctx.SELF, CCostedContext.this.dsl.CostModel.AccessBox);
      def LastBlockUtxoRootHash: Rep[Costed[AvlTree]] = {
        val tree: Rep[AvlTree] = CCostedContext.this.ctx.LastBlockUtxoRootHash;
        RCCostedPrim(tree, CCostedContext.this.dsl.CostModel.AccessAvlTree, tree.dataSize)
      };
      def minerPubKey: Rep[CostedColl[Byte]] = CCostedContext.this.dsl.costColWithConstSizedItem[Byte](CCostedContext.this.ctx.minerPubKey, CCostedContext.this.dsl.CostModel.PubKeySize.toInt, toRep(1L.asInstanceOf[Long]));
      def getVar[T](id: Rep[Byte])(implicit cT: Elem[T]): Rep[CostedOption[T]] = {
        val opt: Rep[WOption[T]] = CCostedContext.this.ctx.getVar[T](id);
        CCostedContext.this.dsl.costOption[T](opt, CCostedContext.this.dsl.CostModel.GetVar)
      };
      def value: Rep[Context] = CCostedContext.this.ctx;
      def cost: Rep[Int] = CCostedContext.this.ctx.cost;
      def dataSize: Rep[Long] = CCostedContext.this.ctx.dataSize;
      def selfBoxIndex: Rep[Costed[Int]] = {
        val cost: Rep[Int] = CCostedContext.this.dsl.CostModel.SelectField;
        RCCostedPrim(CCostedContext.this.ctx.selfBoxIndex, cost, toRep(4L.asInstanceOf[Long]))
      };
      @NeverInline def headers: Rep[CostedColl[Header]] = delayInvoke;
      @NeverInline def preHeader: Rep[Costed[PreHeader]] = delayInvoke
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
    trait CCostedContextCompanion;
    trait CCostedBoxCompanion
  }
}