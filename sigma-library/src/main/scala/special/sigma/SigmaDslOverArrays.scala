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
    import WBigInteger._;
    import WECPoint._;
    import WOption._;
    import WSpecialPredef._; // manual fix
    abstract class TestSigmaDslBuilder extends SigmaDslBuilder {
      def Colls: Rep[CollBuilder] = RCollOverArrayBuilder();
      def Monoids: Rep[MonoidBuilder] = RMonoidBuilderInst();
      def Costing: Rep[CostedBuilder] = RCCostedBuilder();
      @NeverInline def CostModel: Rep[CostModel] = delayInvoke;
      @NeverInline def verifyZK(proof: Rep[Thunk[SigmaProp]]): Rep[Boolean] = delayInvoke;
      @NeverInline def atLeast(bound: Rep[Int], props: Rep[Coll[SigmaProp]]): Rep[SigmaProp] = delayInvoke;
      @NeverInline def allOf(conditions: Rep[Coll[Boolean]]): Rep[Boolean] = delayInvoke;
      @NeverInline def anyOf(conditions: Rep[Coll[Boolean]]): Rep[Boolean] = delayInvoke;
      @NeverInline def allZK(props: Rep[Coll[SigmaProp]]): Rep[SigmaProp] = delayInvoke;
      @NeverInline def anyZK(props: Rep[Coll[SigmaProp]]): Rep[SigmaProp] = delayInvoke;
      @NeverInline override def xorOf(conditions: Rep[Coll[Boolean]]): Rep[Boolean] = delayInvoke;
      @NeverInline def sigmaProp(b: Rep[Boolean]): Rep[SigmaProp] = delayInvoke;
      @NeverInline def blake2b256(bytes: Rep[Coll[Byte]]): Rep[Coll[Byte]] = delayInvoke;
      @NeverInline def sha256(bytes: Rep[Coll[Byte]]): Rep[Coll[Byte]] = delayInvoke;
      @NeverInline def PubKey(base64String: Rep[String]): Rep[SigmaProp] = delayInvoke;
      @NeverInline def byteArrayToBigInt(bytes: Rep[Coll[Byte]]): Rep[BigInt] = delayInvoke;
      @NeverInline def longToByteArray(l: Rep[Long]): Rep[Coll[Byte]] = delayInvoke;
      @NeverInline def byteArrayToLong(bytes: Rep[Coll[Byte]]): Rep[Long] = delayInvoke;
      @NeverInline def proveDlog(g: Rep[GroupElement]): Rep[SigmaProp] = delayInvoke;
      @NeverInline def proveDHTuple(g: Rep[GroupElement], h: Rep[GroupElement], u: Rep[GroupElement], v: Rep[GroupElement]): Rep[SigmaProp] = delayInvoke;
      @NeverInline def groupGenerator: Rep[GroupElement] = delayInvoke;
      @Reified(value = "T") @NeverInline override def substConstants[T](scriptBytes: Rep[Coll[Byte]], positions: Rep[Coll[Int]], newValues: Rep[Coll[T]])(implicit cT: Elem[T]): Rep[Coll[Byte]] = delayInvoke;
      @NeverInline override def decodePoint(encoded: Rep[Coll[Byte]]): Rep[GroupElement] = delayInvoke;
      @NeverInline override def BigInt(n: Rep[WBigInteger]): Rep[BigInt] = delayInvoke;
      @NeverInline override def toBigInteger(n: Rep[BigInt]): Rep[WBigInteger] = delayInvoke;
      @NeverInline def GroupElement(p: Rep[WECPoint]): Rep[GroupElement] = delayInvoke;
      @NeverInline def toECPoint(ge: Rep[GroupElement]): Rep[WECPoint] = delayInvoke;
      @NeverInline override def avlTree(operationFlags: Rep[Byte], digest: Rep[Coll[Byte]], keyLength: Rep[Int], valueLengthOpt: Rep[WOption[Int]]): Rep[AvlTree] = delayInvoke
    };
    trait TestSigmaDslBuilderCompanion
  }
}