package special.collection {
  import scalan._

  trait Costs extends Base { self: Library =>
    import Coll._;
    import Costed._;
    import CostedBuilder._;
    import CostedColl._;
    import CostedFunc._;
    import CostedOption._;
    import CostedPair._;
    import CostedPrim._;
    import MonoidBuilder._;
    import Size._;
    import SizeColl._;
    import SizeFunc._;
    import SizeOption._;
    import SizePair._;
    import SizePrim._;
    import WOption._;
    import WRType._;
    @WithMethodCallRecognizers trait Costed[Val] extends Def[Costed[Val]] {
      implicit def eVal: Elem[Val];
      def builder: Ref[CostedBuilder];
      def value: Ref[Val];
      def cost: Ref[Int];
      def size: Ref[Size[Val]]
    };
    trait CostedPrim[Val] extends Costed[Val] {
      implicit def eVal: Elem[Val];
      def value: Ref[Val];
      def cost: Ref[Int];
      def size: Ref[Size[Val]]
    };
    trait CostedPair[L, R] extends Costed[scala.Tuple2[L, R]] {
      implicit def eL: Elem[L];
      implicit def eR: Elem[R];
      def l: Ref[Costed[L]];
      def r: Ref[Costed[R]];
      def accCost: Ref[Int]
    };
    trait CostedFunc[Env, Arg, Res] extends Costed[scala.Function1[Arg, Res]] {
      implicit def eEnv: Elem[Env];
      implicit def eArg: Elem[Arg];
      implicit def eRes: Elem[Res];
      def envCosted: Ref[Costed[Env]];
      def func: Ref[scala.Function1[Costed[Arg], Costed[Res]]];
      def cost: Ref[Int];
      def sliceCalc: Ref[scala.Function1[Arg, Res]];
      def sliceCost: Ref[scala.Function1[scala.Tuple2[Int, Size[Arg]], Int]];
      def sliceCostEx: Ref[scala.Function1[scala.Tuple2[Arg, scala.Tuple2[Int, Size[Arg]]], Int]];
      def sliceSize: Ref[scala.Function1[Size[Arg], Size[Res]]]
    };
    @WithMethodCallRecognizers trait CostedColl[Item] extends Costed[Coll[Item]] {
      implicit def eItem: Elem[Item];
      def values: Ref[Coll[Item]];
      def costs: Ref[Coll[Int]];
      def sizes: Ref[Coll[Size[Item]]];
      def valuesCost: Ref[Int];
      def mapCosted[Res](f: Ref[scala.Function1[Costed[Item], Costed[Res]]]): Ref[CostedColl[Res]];
      def filterCosted(f: Ref[scala.Function1[Costed[Item], Costed[Boolean]]]): Ref[CostedColl[Item]];
      def foldCosted[B](zero: Ref[Costed[B]], op: Ref[scala.Function1[Costed[scala.Tuple2[B, Item]], Costed[B]]]): Ref[Costed[B]]
    };
    trait CostedOption[T] extends Costed[WOption[T]] {
      implicit def eT: Elem[T];
      def costOpt: Ref[WOption[Int]];
      def sizeOpt: Ref[WOption[Size[T]]];
      def accumulatedCost: Ref[Int]
    };
    @WithMethodCallRecognizers trait CostedBuilder extends Def[CostedBuilder] {
      def ConstructTupleCost: Ref[Int] = toRep(1.asInstanceOf[Int]);
      def ConstructSumCost: Ref[Int] = toRep(1.asInstanceOf[Int]);
      def SelectFieldCost: Ref[Int] = toRep(1.asInstanceOf[Int]);
      def SumTagSize: Ref[Long] = toRep(1L.asInstanceOf[Long]);
      def costedValue[T](x: Ref[T], optCost: Ref[WOption[Int]]): Ref[Costed[T]];
      def defaultValue[T](valueType: Ref[WRType[T]]): Ref[T];
      def monoidBuilder: Ref[MonoidBuilder];
      def mkSizePrim[T](dataSize: Ref[Long], tT: Ref[WRType[T]]): Ref[SizePrim[T]];
      def mkSizePair[L, R](l: Ref[Size[L]], r: Ref[Size[R]]): Ref[SizePair[L, R]];
      def mkSizeColl[T](sizes: Ref[Coll[Size[T]]]): Ref[SizeColl[T]];
      def mkSizeFunc[E, A, R](sizeEnv: Ref[Size[E]], sizeFunc: Ref[Long], tA: Ref[WRType[A]], tR: Ref[WRType[R]]): Ref[SizeFunc[E, A, R]];
      def mkSizeOption[T](sizeOpt: Ref[WOption[Size[T]]]): Ref[SizeOption[T]];
      def mkCostedPrim[T](value: Ref[T], cost: Ref[Int], size: Ref[Size[T]]): Ref[CostedPrim[T]];
      def mkCostedPair[L, R](first: Ref[Costed[L]], second: Ref[Costed[R]], accCost: Ref[Int]): Ref[CostedPair[L, R]];
      def mkCostedFunc[Env, Arg, Res](envCosted: Ref[Costed[Env]], func: Ref[scala.Function1[Costed[Arg], Costed[Res]]], cost: Ref[Int], size: Ref[Size[scala.Function1[Arg, Res]]]): Ref[CostedFunc[Env, Arg, Res]];
      def mkCostedColl[T](values: Ref[Coll[T]], costs: Ref[Coll[Int]], sizes: Ref[Coll[Size[T]]], valuesCost: Ref[Int]): Ref[CostedColl[T]];
      def mkCostedOption[T](value: Ref[WOption[T]], costOpt: Ref[WOption[Int]], sizeOpt: Ref[WOption[Size[T]]], accumulatedCost: Ref[Int]): Ref[CostedOption[T]]
    };
    trait CostedCompanion;
    trait CostedPrimCompanion;
    trait CostedPairCompanion;
    trait CostedFuncCompanion;
    trait CostedCollCompanion;
    trait CostedOptionCompanion;
    trait CostedBuilderCompanion
  }
}