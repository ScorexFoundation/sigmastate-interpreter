package special.sigma {
  import scalan._

  trait SigmaDslCosted extends Base { self: SigmaLibrary =>
    import AnyValue._;
    import AvlTree._;
    import Box._;
    import CSizeAnyValue._;
    import CSizeBox._;
    import CSizeContext._;
    import Coll._;
    import Header._;
    import PreHeader._;
    import Size._;
    import SizeAnyValue._;
    import SizeBox._;
    import SizeBuilder._;
    import SizeContext._;
    import SizeSigmaProp._;
    import WOption._;
    import WRType._;
    abstract class CSizeAnyValue(val tVal: Ref[WRType[Any]], val valueSize: Ref[Size[Any]]) extends SizeAnyValue {
      @NeverInline override def dataSize: Ref[Long] = delayInvoke
    };
    abstract class CSizeSigmaProp(val propBytes: Ref[Size[Coll[Byte]]]) extends SizeSigmaProp {
      @NeverInline override def dataSize: Ref[Long] = delayInvoke
    };
    abstract class CSizeBox(val propositionBytes: Ref[Size[Coll[Byte]]], val bytes: Ref[Size[Coll[Byte]]], val bytesWithoutRef: Ref[Size[Coll[Byte]]], val registers: Ref[Size[Coll[WOption[AnyValue]]]], val tokens: Ref[Size[Coll[scala.Tuple2[Coll[Byte], Long]]]]) extends SizeBox {
      @NeverInline override def dataSize: Ref[Long] = delayInvoke;
      @NeverInline override def getReg[T](id: Ref[Byte])(implicit tT: Elem[T]): Ref[Size[WOption[T]]] = delayInvoke
    };
    abstract class CSizeContext(val outputs: Ref[Size[Coll[Box]]], val inputs: Ref[Size[Coll[Box]]], val dataInputs: Ref[Size[Coll[Box]]], val selfBox: Ref[Size[Box]], val lastBlockUtxoRootHash: Ref[Size[AvlTree]], val headers: Ref[Size[Coll[Header]]], val preHeader: Ref[Size[PreHeader]], val vars: Ref[Coll[Size[AnyValue]]]) extends SizeContext {
      @NeverInline override def dataSize: Ref[Long] = delayInvoke;
      @NeverInline override def getVar[T](id: Ref[Byte])(implicit tT: Elem[T]): Ref[Size[WOption[T]]] = delayInvoke
    };
    abstract class CSizeBuilder extends SizeBuilder {
      def mkSizeAnyValue(tVal: Ref[WRType[Any]], valueSize: Ref[Size[Any]]): Ref[SizeAnyValue] = RCSizeAnyValue(tVal, valueSize);
      def mkSizeBox(propositionBytes: Ref[Size[Coll[Byte]]], bytes: Ref[Size[Coll[Byte]]], bytesWithoutRef: Ref[Size[Coll[Byte]]], registers: Ref[Size[Coll[WOption[AnyValue]]]], tokens: Ref[Size[Coll[scala.Tuple2[Coll[Byte], Long]]]]): Ref[SizeBox] = RCSizeBox(propositionBytes, bytes, bytesWithoutRef, registers, tokens);
      def mkSizeContext(outputs: Ref[Size[Coll[Box]]], inputs: Ref[Size[Coll[Box]]], dataInputs: Ref[Size[Coll[Box]]], selfBox: Ref[Size[Box]], lastBlockUtxoRootHash: Ref[Size[AvlTree]], headers: Ref[Size[Coll[Header]]], preHeader: Ref[Size[PreHeader]], vars: Ref[Coll[Size[AnyValue]]]): Ref[SizeContext] = RCSizeContext(outputs, inputs, dataInputs, selfBox, lastBlockUtxoRootHash, headers, preHeader, vars)
    };
    trait CSizeAnyValueCompanion;
    trait CSizeSigmaPropCompanion;
    trait CSizeBoxCompanion;
    trait CSizeContextCompanion;
    trait CSizeBuilderCompanion
  }
}