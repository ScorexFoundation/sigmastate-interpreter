package special.sigma {
  import scalan._

  trait CostedObjects extends Base { self: SigmaLibrary =>
    import AnyValue._;
    import AvlTree._;
    import Box._;
    import Coll._;
    import Context._;
    import Header._;
    import PreHeader._;
    import SigmaProp._;
    import Size._;
    import SizeAnyValue._;
    import SizeBox._;
    import SizeBuilder._;
    import SizeContext._;
    import WOption._;
    import WRType._;
    @Liftable @WithMethodCallRecognizers trait SizeAnyValue extends Size[AnyValue] {
      def tVal: Ref[WRType[Any]];
      def valueSize: Ref[Size[Any]]
    };
    @Liftable @WithMethodCallRecognizers trait SizeSigmaProp extends Size[SigmaProp] {
      def propBytes: Ref[Size[Coll[Byte]]]
    };
    @Liftable @WithMethodCallRecognizers trait SizeBox extends Size[Box] {
      def propositionBytes: Ref[Size[Coll[Byte]]];
      def bytes: Ref[Size[Coll[Byte]]];
      def bytesWithoutRef: Ref[Size[Coll[Byte]]];
      def registers: Ref[Size[Coll[WOption[AnyValue]]]];
      def getReg[T](id: Ref[Byte])(implicit tT: Elem[T]): Ref[Size[WOption[T]]];
      def tokens: Ref[Size[Coll[scala.Tuple2[Coll[Byte], Long]]]]
    };
    @Liftable @WithMethodCallRecognizers trait SizeContext extends Size[Context] {
      def outputs: Ref[Size[Coll[Box]]];
      def inputs: Ref[Size[Coll[Box]]];
      def dataInputs: Ref[Size[Coll[Box]]];
      def selfBox: Ref[Size[Box]];
      def lastBlockUtxoRootHash: Ref[Size[AvlTree]];
      def headers: Ref[Size[Coll[Header]]];
      def preHeader: Ref[Size[PreHeader]];
      def getVar[T](id: Ref[Byte])(implicit tT: Elem[T]): Ref[Size[WOption[T]]]
    };
    @Liftable trait SizeBuilder extends Def[SizeBuilder] {
      def mkSizeAnyValue(tVal: Ref[WRType[Any]], valueSize: Ref[Size[Any]]): Ref[SizeAnyValue];
      def mkSizeBox(propositionBytes: Ref[Size[Coll[Byte]]], bytes: Ref[Size[Coll[Byte]]], bytesWithoutRef: Ref[Size[Coll[Byte]]], registers: Ref[Size[Coll[WOption[AnyValue]]]], tokens: Ref[Size[Coll[scala.Tuple2[Coll[Byte], Long]]]]): Ref[SizeBox];
      def mkSizeContext(outputs: Ref[Size[Coll[Box]]], inputs: Ref[Size[Coll[Box]]], dataInputs: Ref[Size[Coll[Box]]], selfBox: Ref[Size[Box]], lastBlockUtxoRootHash: Ref[Size[AvlTree]], headers: Ref[Size[Coll[Header]]], preHeader: Ref[Size[PreHeader]], vars: Ref[Coll[Size[AnyValue]]]): Ref[SizeContext]
    };
    trait SizeAnyValueCompanion;
    trait SizeSigmaPropCompanion;
    trait SizeBoxCompanion;
    trait SizeContextCompanion;
    trait SizeBuilderCompanion
  }
}