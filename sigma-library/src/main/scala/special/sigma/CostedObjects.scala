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
    import Size._;
    import SizeAnyValue._;
    import SizeBox._;
    import SizeBuilder._;
    import SizeContext._;
    import WOption._;
    import WRType._;
    @Liftable trait SizeAnyValue extends Size[AnyValue] {
      def tVal: Rep[WRType[Any]];
      def valueSize: Rep[Size[Any]]
    };
    @Liftable trait SizeBox extends Size[Box] {
      def propositionBytes: Rep[Size[Coll[Byte]]];
      def bytes: Rep[Size[Coll[Byte]]];
      def bytesWithoutRef: Rep[Size[Coll[Byte]]];
      def registers: Rep[Size[Coll[WOption[AnyValue]]]]
    };
    @Liftable trait SizeContext extends Size[Context] {
      def outputs: Rep[Size[Coll[Box]]];
      def inputs: Rep[Size[Coll[Box]]];
      def dataInputs: Rep[Size[Coll[Box]]];
      def selfBox: Rep[Size[Box]];
      def lastBlockUtxoRootHash: Rep[Size[AvlTree]];
      def headers: Rep[Size[Coll[Header]]];
      def preHeader: Rep[Size[PreHeader]]
    };
    @Liftable trait SizeBuilder extends Def[SizeBuilder] {
      def mkSizeAnyValue(tVal: Rep[WRType[Any]], valueSize: Rep[Size[Any]]): Rep[SizeAnyValue];
      def mkSizeBox(propositionBytes: Rep[Size[Coll[Byte]]], bytes: Rep[Size[Coll[Byte]]], bytesWithoutRef: Rep[Size[Coll[Byte]]], registers: Rep[Size[Coll[WOption[AnyValue]]]]): Rep[SizeBox];
      def mkSizeContext(outputs: Rep[Size[Coll[Box]]], inputs: Rep[Size[Coll[Box]]], dataInputs: Rep[Size[Coll[Box]]], selfBox: Rep[Size[Box]], lastBlockUtxoRootHash: Rep[Size[AvlTree]], headers: Rep[Size[Coll[Header]]], preHeader: Rep[Size[PreHeader]]): Rep[SizeContext]
    };
    trait SizeAnyValueCompanion;
    trait SizeBoxCompanion;
    trait SizeContextCompanion;
    trait SizeBuilderCompanion
  }
}