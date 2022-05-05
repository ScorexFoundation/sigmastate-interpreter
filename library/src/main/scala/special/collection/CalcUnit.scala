package special.collection {
  import scalan._

  trait Calcs extends Base { self: Library =>
    import Coll._;
    // import Costed._;
    // import CostedBuilder._;
    // import CostedColl._;
    // import CostedFunc._;
    // import CostedOption._;
    // import CostedPair._;
    // import CostedPrim._;
    import MonoidBuilder._;
    import Size._;
    import SizeColl._;
    import SizeFunc._;
    import SizeOption._;
    import SizePair._;
    import SizePrim._;
    import WOption._;
    import WRType._;
    
    @WithMethodCallRecognizers
    trait Calc[Val] extends Def[Calc[Val]] {
      implicit def eVal: Elem[Val];
      def builder: Ref[CalcBuilder];
      def value: Ref[Val];
      def cost: Ref[Int];
      def size: Ref[Size[Val]]
    }

    trait CalcPrim[Val] extends Calc[Val] {
      implicit def eVal: Elem[Val];
      def value: Ref[Val];
      def cost: Ref[Int];
      def size: Ref[Size[Val]]
    }

    trait CalcPair[L, R] extends Calc[scala.Tuple2[L, R]] {
      implicit def eL: Elem[L];
      implicit def eR: Elem[R];
      def l: Ref[Calc[L]];
      def r: Ref[Calc[R]];
      def accCost: Ref[Int]
    }

    trait CalcFunc[Env, Arg, Res] extends Calc[scala.Function1[Arg, Res]] {
      implicit def eEnv: Elem[Env];
      implicit def eArg: Elem[Arg];
      implicit def eRes: Elem[Res];
      def envCosted: Ref[Calc[Env]];
      def func: Ref[scala.Function1[Calc[Arg], Calc[Res]]];
      def cost: Ref[Int];
      def sliceCalc: Ref[scala.Function1[Arg, Res]];
      def sliceCost: Ref[scala.Function1[scala.Tuple2[Int, Size[Arg]], Int]];
      def sliceCostEx: Ref[scala.Function1[scala.Tuple2[Arg, scala.Tuple2[Int, Size[Arg]]], Int]];
      def sliceSize: Ref[scala.Function1[Size[Arg], Size[Res]]]
    }

    @WithMethodCallRecognizers
    trait CalcColl[Item] extends Calc[Coll[Item]] {
      implicit def eItem: Elem[Item];
      def values: Ref[Coll[Item]];
      def costs: Ref[Coll[Int]];
      def sizes: Ref[Coll[Size[Item]]];
      def valuesCost: Ref[Int];
      def mapCosted[Res](f: Ref[scala.Function1[Calc[Item], Calc[Res]]]): Ref[CalcColl[Res]];
      def filterCosted(f: Ref[scala.Function1[Calc[Item], Calc[Boolean]]]): Ref[CalcColl[Item]];
      def foldCosted[B](zero: Ref[Calc[B]], op: Ref[scala.Function1[Calc[scala.Tuple2[B, Item]], Calc[B]]]): Ref[Calc[B]]
    }

    trait CalcOption[T] extends Calc[WOption[T]] {
      implicit def eT: Elem[T];
      def costOpt: Ref[WOption[Int]];
      def sizeOpt: Ref[WOption[Size[T]]];
      def accumulatedCost: Ref[Int]
    }

    @WithMethodCallRecognizers
    trait CalcBuilder extends Def[CalcBuilder] {
      def ConstructTupleCost: Ref[Int] = toRep(1.asInstanceOf[Int]);
      def ConstructSumCost: Ref[Int] = toRep(1.asInstanceOf[Int]);
      def SelectFieldCost: Ref[Int] = toRep(1.asInstanceOf[Int]);
      def SumTagSize: Ref[Long] = toRep(1L.asInstanceOf[Long]);
      def calcValue[T](x: Ref[T], optCalc: Ref[WOption[Int]]): Ref[Calc[T]];
      def defaultValue[T](valueType: Ref[WRType[T]]): Ref[T];
      def monoidBuilder: Ref[MonoidBuilder];
      def mkSizePrim[T](dataSize: Ref[Long], tT: Ref[WRType[T]]): Ref[SizePrim[T]];
      def mkSizePair[L, R](l: Ref[Size[L]], r: Ref[Size[R]]): Ref[SizePair[L, R]];
      def mkSizeColl[T](sizes: Ref[Coll[Size[T]]]): Ref[SizeColl[T]];
      def mkSizeFunc[E, A, R](sizeEnv: Ref[Size[E]], sizeFunc: Ref[Long], tA: Ref[WRType[A]], tR: Ref[WRType[R]]): Ref[SizeFunc[E, A, R]];
      def mkSizeOption[T](sizeOpt: Ref[WOption[Size[T]]]): Ref[SizeOption[T]];
      def mkCalcPrim[T](value: Ref[T], size: Ref[Size[T]]): Ref[CalcPrim[T]];
      def mkCalcPair[L, R](first: Ref[Calc[L]], second: Ref[Calc[R]]): Ref[CalcPair[L, R]];
      def mkCalcFunc[Env, Arg, Res](envCosted: Ref[Calc[Env]], func: Ref[scala.Function1[Calc[Arg], Calc[Res]]], size: Ref[Size[scala.Function1[Arg, Res]]]): Ref[CalcFunc[Env, Arg, Res]];
      def mkCalcColl[T](values: Ref[Coll[T]], sizes: Ref[Coll[Size[T]]], valuesCost: Ref[Int]): Ref[CalcColl[T]];
      def mkCalcOption[T](value: Ref[WOption[T]], sizeOpt: Ref[WOption[Size[T]]]): Ref[CalcOption[T]]
    }
    
    trait CalcCompanion
    trait CalcPrimCompanion
    trait CalcPairCompanion
    trait CalcFuncCompanion
    trait CalcCollCompanion
    trait CalcOptionCompanion
    trait CalcBuilderCompanion
  }
}