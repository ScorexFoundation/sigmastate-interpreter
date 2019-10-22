package special.collection {
  import scalan._

  trait Sizes extends Base { self: Library =>
    import Coll._;
    import Size._;
    import WOption._;
    import WRType._;
    @Liftable @WithMethodCallRecognizers trait Size[Val] extends Def[Size[Val]] {
      implicit def eVal: Elem[Val];
      def dataSize: Ref[Long]
    };
    @Liftable trait SizePrim[Val] extends Size[Val] {
      implicit def eVal: Elem[Val];
      def dataSize: Ref[Long];
      def tVal: Ref[WRType[Val]]
    };
    @Liftable @WithMethodCallRecognizers trait SizePair[L, R] extends Size[scala.Tuple2[L, R]] {
      implicit def eL: Elem[L];
      implicit def eR: Elem[R];
      def l: Ref[Size[L]];
      def r: Ref[Size[R]]
    };
    @Liftable @WithMethodCallRecognizers trait SizeColl[Item] extends Size[Coll[Item]] {
      implicit def eItem: Elem[Item];
      def sizes: Ref[Coll[Size[Item]]]
    };
    @Liftable @WithMethodCallRecognizers trait SizeFunc[Env, Arg, Res] extends Size[scala.Function1[Arg, Res]] {
      implicit def eEnv: Elem[Env];
      implicit def eArg: Elem[Arg];
      implicit def eRes: Elem[Res];
      def sizeEnv: Ref[Size[Env]]
    };
    @Liftable @WithMethodCallRecognizers trait SizeOption[T] extends Size[WOption[T]] {
      implicit def eT: Elem[T];
      def sizeOpt: Ref[WOption[Size[T]]]
    };
    trait SizeCompanion;
    trait SizePrimCompanion;
    trait SizePairCompanion;
    trait SizeCollCompanion;
    trait SizeFuncCompanion;
    trait SizeOptionCompanion
  }
}