package special.sigma {
  import scalan._

  trait CostedObjects extends Base { self: SigmaLibrary =>
    import AnyValue._;
    import AvlTree._;
    import Box._;
    import Coll._;
    import Context._;
    import Costed._;
    import CostedBox._;
    import CostedBuilder._;
    import CostedColl._;
    import CostedOption._;
    import CostedSigmaObject._;
    import Header._;
    import PreHeader._;
    import SigmaDslBuilder._;
    trait CostedSigmaObject[Val] extends Costed[Val] {
      implicit def eVal: Elem[Val];
      def dsl: Rep[SigmaDslBuilder];
      def builder: Rep[CostedBuilder] = CostedSigmaObject.this.dsl.Costing
    };
    trait CostedContext extends CostedSigmaObject[Context] {
      def dataInputs: Rep[CostedColl[Box]];
      def OUTPUTS: Rep[CostedColl[Box]];
      def INPUTS: Rep[CostedColl[Box]];
      def HEIGHT: Rep[Costed[Int]];
      def SELF: Rep[CostedBox];
      def selfBoxIndex: Rep[Costed[Int]];
      def LastBlockUtxoRootHash: Rep[Costed[AvlTree]];
      def headers: Rep[CostedColl[Header]];
      def preHeader: Rep[Costed[PreHeader]];
      def minerPubKey: Rep[CostedColl[Byte]];
      def getVar[T](id: Rep[Byte])(implicit cT: Elem[T]): Rep[CostedOption[T]]
    };
    trait CostedBox extends CostedSigmaObject[Box] {
      def id: Rep[CostedColl[Byte]];
      def valueCosted: Rep[Costed[Long]];
      def bytes: Rep[CostedColl[Byte]];
      def bytesWithoutRef: Rep[CostedColl[Byte]];
      def propositionBytes: Rep[CostedColl[Byte]];
      def registers: Rep[CostedColl[AnyValue]];
      def getReg[T](id: Rep[Int])(implicit cT: Elem[T]): Rep[CostedOption[T]];
      def creationInfo: Rep[Costed[scala.Tuple2[Int, Coll[Byte]]]]
    };
    trait CostedSigmaObjectCompanion;
    trait CostedContextCompanion;
    trait CostedBoxCompanion
  }
}