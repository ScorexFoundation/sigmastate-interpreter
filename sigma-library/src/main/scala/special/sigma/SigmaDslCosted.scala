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
    abstract class CSizeAnyValue(val tVal: Rep[WRType[Any]], val valueSize: Rep[Size[Any]]) extends SizeAnyValue {
      @NeverInline override def dataSize: Rep[Long] = delayInvoke
    };
    abstract class CSizeSigmaProp(val propBytes: Rep[Size[Coll[Byte]]]) extends SizeSigmaProp {
      @NeverInline override def dataSize: Rep[Long] = delayInvoke
    };
    abstract class CSizeBox(val propositionBytes: Rep[Size[Coll[Byte]]], val bytes: Rep[Size[Coll[Byte]]], val bytesWithoutRef: Rep[Size[Coll[Byte]]], val registers: Rep[Size[Coll[WOption[AnyValue]]]], val tokens: Rep[Size[Coll[scala.Tuple2[Coll[Byte], Long]]]]) extends SizeBox {
      @NeverInline override def dataSize: Rep[Long] = delayInvoke;
      @NeverInline override def getReg[T](id: Rep[Byte])(implicit tT: Elem[T]): Rep[Size[WOption[T]]] = delayInvoke
    };
    abstract class CSizeContext(val outputs: Rep[Size[Coll[Box]]], val inputs: Rep[Size[Coll[Box]]], val dataInputs: Rep[Size[Coll[Box]]], val selfBox: Rep[Size[Box]], val lastBlockUtxoRootHash: Rep[Size[AvlTree]], val headers: Rep[Size[Coll[Header]]], val preHeader: Rep[Size[PreHeader]], val vars: Rep[Coll[Size[AnyValue]]]) extends SizeContext {
      @NeverInline override def dataSize: Rep[Long] = delayInvoke;
      @NeverInline override def getVar[T](id: Rep[Byte])(implicit tT: Elem[T]): Rep[Size[WOption[T]]] = delayInvoke
    };
    abstract class CSizeBuilder extends SizeBuilder {
      def mkSizeAnyValue(tVal: Rep[WRType[Any]], valueSize: Rep[Size[Any]]): Rep[SizeAnyValue] = RCSizeAnyValue(tVal, valueSize);
      def mkSizeBox(propositionBytes: Rep[Size[Coll[Byte]]], bytes: Rep[Size[Coll[Byte]]], bytesWithoutRef: Rep[Size[Coll[Byte]]], registers: Rep[Size[Coll[WOption[AnyValue]]]], tokens: Rep[Size[Coll[scala.Tuple2[Coll[Byte], Long]]]]): Rep[SizeBox] = RCSizeBox(propositionBytes, bytes, bytesWithoutRef, registers, tokens);
      def mkSizeContext(outputs: Rep[Size[Coll[Box]]], inputs: Rep[Size[Coll[Box]]], dataInputs: Rep[Size[Coll[Box]]], selfBox: Rep[Size[Box]], lastBlockUtxoRootHash: Rep[Size[AvlTree]], headers: Rep[Size[Coll[Header]]], preHeader: Rep[Size[PreHeader]], vars: Rep[Coll[Size[AnyValue]]]): Rep[SizeContext] = RCSizeContext(outputs, inputs, dataInputs, selfBox, lastBlockUtxoRootHash, headers, preHeader, vars)
    };
    trait CSizeAnyValueCompanion;
    trait CSizeSigmaPropCompanion;
    trait CSizeBoxCompanion;
    trait CSizeContextCompanion;
    trait CSizeBuilderCompanion
  }
}