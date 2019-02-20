package special.sigma {
  import scalan.OverloadHack.Overloaded1  // manual fix
  import scalan._

  trait SigmaDslOverArrays extends Base { self: SigmaLibrary =>
    import AvlTree._;
    import BigInt._;
    import Box._;
    import CostedBuilder._  // manual fix
    import CCostedBuilder._;
    import Coll._;
    import CollBuilder._;
    import CollOverArrayBuilder._;
    import CostModel._;
    import Costed._;
    import CostedBuilder._;
    import CostedColl._;
    import CostedOption._;
    import GroupElement._;
    import MonoidBuilder._;
    import MonoidBuilderInst._;
    import SigmaDslBuilder._;
    import SigmaProp._;
    import TestSigmaDslBuilder._;
    import WBigInteger._;
    import WECPoint._;
    import WOption._;
    import CostedNone._; // manual fix
    import CostedSome._; // manuaf fix
    import WSpecialPredef._;
    abstract class TestAvlTree(val startingDigest: Rep[Coll[Byte]], val keyLength: Rep[Int], val valueLengthOpt: Rep[WOption[Int]], val maxNumOperations: Rep[WOption[Int]], val maxDeletes: Rep[WOption[Int]]) extends AvlTree with Product with Serializable {
      def builder: Rep[TestSigmaDslBuilder] = RTestSigmaDslBuilder();
      @NeverInline def dataSize: Rep[Long] = delayInvoke;
      @NeverInline def cost: Rep[Int] = delayInvoke;
      @NeverInline def digest: Rep[Coll[Byte]] = delayInvoke
    };
    abstract class TestSigmaDslBuilder extends SigmaDslBuilder {
      def Colls: Rep[CollBuilder] = RCollOverArrayBuilder();
      def Monoids: Rep[MonoidBuilder] = RMonoidBuilderInst();
      def Costing: Rep[CostedBuilder] = RCCostedBuilder();
      @NeverInline def CostModel: Rep[CostModel] = delayInvoke;
      def costBoxes(bs: Rep[Coll[Box]]): Rep[CostedColl[Box]] = {
        val len: Rep[Int] = bs.length;
        val perItemCost: Rep[Int] = this.CostModel.AccessBox;
        val costs: Rep[Coll[Int]] = this.Colls.replicate[Int](len, perItemCost);
        val sizes: Rep[Coll[Long]] = bs.map[Long](fun(((b: Rep[Box]) => b.dataSize)));
        val valuesCost: Rep[Int] = this.CostModel.CollectionConst;
        this.Costing.mkCostedColl[Box](bs, costs, sizes, valuesCost)
      };
      def costColWithConstSizedItem[T](xs: Rep[Coll[T]], len: Rep[Int], itemSize: Rep[Long]): Rep[CostedColl[T]] = {
        // manual fix (div)
        val perItemCost: Rep[Long] = len.toLong.*(itemSize).div(toRep(1024L.asInstanceOf[Long])).+(toRep(1.asInstanceOf[Long])).*(this.CostModel.AccessKiloByteOfData.toLong);
        val costs: Rep[Coll[Int]] = this.Colls.replicate[Int](len, perItemCost.toInt);
        val sizes: Rep[Coll[Long]] = this.Colls.replicate[Long](len, itemSize);
        val valueCost: Rep[Int] = this.CostModel.CollectionConst;
        this.Costing.mkCostedColl[T](xs, costs, sizes, valueCost)
      };
      def costOption[T](opt: Rep[WOption[T]], opCost: Rep[Int]): Rep[CostedOption[T]] = {
        implicit val eT = opt.elem.eItem
        val none = Thunk(RCostedNone[T](opCost));
        opt.fold[CostedOption[T]](none,
          fun(((x: Rep[T]) => this.Costing.mkCostedSome[T](this.Costing.costedValue[T](x, RWSpecialPredef.some[Int](opCost))))))
      };
      @NeverInline def verifyZK(proof: Rep[Thunk[SigmaProp]]): Rep[Boolean] = delayInvoke;
      @NeverInline def atLeast(bound: Rep[Int], props: Rep[Coll[SigmaProp]]): Rep[SigmaProp] = delayInvoke;
      @NeverInline def allOf(conditions: Rep[Coll[Boolean]]): Rep[Boolean] = delayInvoke;
      @NeverInline def anyOf(conditions: Rep[Coll[Boolean]]): Rep[Boolean] = delayInvoke;
      @NeverInline def allZK(props: Rep[Coll[SigmaProp]]): Rep[SigmaProp] = delayInvoke;
      @NeverInline def anyZK(props: Rep[Coll[SigmaProp]]): Rep[SigmaProp] = delayInvoke;
      @NeverInline def sigmaProp(b: Rep[Boolean]): Rep[SigmaProp] = delayInvoke;
      @NeverInline def blake2b256(bytes: Rep[Coll[Byte]]): Rep[Coll[Byte]] = delayInvoke;
      @NeverInline def sha256(bytes: Rep[Coll[Byte]]): Rep[Coll[Byte]] = delayInvoke;
      @NeverInline def PubKey(base64String: Rep[String]): Rep[SigmaProp] = delayInvoke;
      @NeverInline def byteArrayToBigInt(bytes: Rep[Coll[Byte]]): Rep[BigInt] = delayInvoke;
      @NeverInline def longToByteArray(l: Rep[Long]): Rep[Coll[Byte]] = delayInvoke;
      @NeverInline def proveDlog(g: Rep[GroupElement]): Rep[SigmaProp] = delayInvoke;
      @NeverInline def proveDHTuple(g: Rep[GroupElement], h: Rep[GroupElement], u: Rep[GroupElement], v: Rep[GroupElement]): Rep[SigmaProp] = delayInvoke;
      @NeverInline def isMember(tree: Rep[AvlTree], key: Rep[Coll[Byte]], proof: Rep[Coll[Byte]]): Rep[Boolean] = delayInvoke;
      @NeverInline def treeLookup(tree: Rep[AvlTree], key: Rep[Coll[Byte]], proof: Rep[Coll[Byte]]): Rep[WOption[Coll[Byte]]] = delayInvoke;
      @NeverInline def treeModifications(tree: Rep[AvlTree], operations: Rep[Coll[Byte]], proof: Rep[Coll[Byte]]): Rep[WOption[AvlTree]] = delayInvoke;
      @NeverInline def groupGenerator: Rep[GroupElement] = delayInvoke;
      @Reified(value = "T") @NeverInline override def substConstants[T](scriptBytes: Rep[Coll[Byte]], positions: Rep[Coll[Int]], newValues: Rep[Coll[T]])(implicit cT: Elem[T]): Rep[Coll[Byte]] = delayInvoke;
      @NeverInline override def decodePoint(encoded: Rep[Coll[Byte]]): Rep[GroupElement] = delayInvoke;
      @NeverInline override def BigInt(n: Rep[WBigInteger]): Rep[BigInt] = delayInvoke;
      @NeverInline override def toBigInteger(n: Rep[BigInt]): Rep[WBigInteger] = delayInvoke;
      @NeverInline def GroupElement(p: Rep[WECPoint]): Rep[GroupElement] = delayInvoke;
      @NeverInline def toECPoint(ge: Rep[GroupElement]): Rep[WECPoint] = delayInvoke
    };
    trait TestAvlTreeCompanion;
    trait TestSigmaDslBuilderCompanion
  }
}