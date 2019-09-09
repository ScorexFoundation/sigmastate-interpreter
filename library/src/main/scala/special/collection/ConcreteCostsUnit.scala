package special.collection {
  import scalan._

  trait ConcreteCosts extends Base { self: Library =>
    import CCostedBuilder._;
    import CCostedColl._;
    import CCostedFunc._;
    import CCostedOption._;
    import CCostedPair._;
    import CCostedPrim._;
    import CSizeColl._;
    import CSizeFunc._;
    import CSizeOption._;
    import CSizePair._;
    import CSizePrim._;
    import Coll._;
    import Costed._;
    import CostedBuilder._;
    import CostedColl._;
    import CostedFunc._;
    import CostedOption._;
    import CostedPair._;
    import CostedPrim._;
    import MonoidBuilder._;
    import MonoidBuilderInst._;
    import Size._;
    import SizeColl._;
    import SizeFunc._;
    import SizeOption._;
    import SizePair._;
    import SizePrim._;
    import WOption._;
    import WRType._;
    import WSpecialPredef._;
    abstract class CCostedPrim[Val](val value: Ref[Val], val cost: Ref[Int], val size: Ref[Size[Val]]) extends CostedPrim[Val] {
      def builder: Ref[CostedBuilder] = RCCostedBuilder()
    };
    abstract class CCostedPair[L, R](val l: Ref[Costed[L]], val r: Ref[Costed[R]], val accCost: Ref[Int]) extends CostedPair[L, R] {
      def builder: Ref[CostedBuilder] = RCCostedBuilder();
      def value: Ref[scala.Tuple2[L, R]] = Pair(CCostedPair.this.l.value, CCostedPair.this.r.value);
      @NeverInline def cost: Ref[Int] = delayInvoke;
      def size: Ref[Size[scala.Tuple2[L, R]]] = CCostedPair.this.builder.mkSizePair[L, R](CCostedPair.this.l.size, CCostedPair.this.r.size)
    };
    abstract class CCostedFunc[Env, Arg, Res](val envCosted: Ref[Costed[Env]], val func: Ref[scala.Function1[Costed[Arg], Costed[Res]]], val cost: Ref[Int], val size: Ref[Size[scala.Function1[Arg, Res]]]) extends CostedFunc[Env, Arg, Res] {
      def builder: Ref[CostedBuilder] = RCCostedBuilder();
      @NeverInline def value: Ref[scala.Function1[Arg, Res]] = delayInvoke

      // manual fix
      lazy val sliceCalc: Ref[Arg => Res] = fun { x: Ref[Arg] => func(RCCostedPrim(x, 0, zeroSize(x.elem))).value }

      // manual fix
      lazy val sliceCost: Ref[((Int,Size[Arg])) => Int] = fun { in: Ref[(Int, Size[Arg])] =>
        val Pair(c, s) = in
        func(RCCostedPrim(placeholder[Arg], c, s)).cost
      }

      // manual fix
      lazy val sliceCostEx: Ref[((Arg, (Int,Size[Arg]))) => Int] = fun { in: Ref[(Arg, (Int, Size[Arg]))] =>
        val Pair(ctx, Pair(c, s)) = in
        func(RCCostedPrim(ctx, c, s)).cost
      }

      // manual fix
      lazy val sliceSize: Ref[Size[Arg] => Size[Res]] = fun { in: Ref[Size[Arg]] =>
        val s = in
        val arg = RCCostedPrim(placeholder[Arg], 0, s)
        func(arg).size
      }
    };
    abstract class CCostedColl[Item](val values: Ref[Coll[Item]], val costs: Ref[Coll[Int]], val sizes: Ref[Coll[Size[Item]]], val valuesCost: Ref[Int]) extends CostedColl[Item] {
      def builder: Ref[CostedBuilder] = RCCostedBuilder();
      def value: Ref[Coll[Item]] = CCostedColl.this.values;
      @NeverInline def cost: Ref[Int] = delayInvoke;
      def size: Ref[Size[Coll[Item]]] = CCostedColl.this.builder.mkSizeColl[Item](CCostedColl.this.sizes);
      @NeverInline def mapCosted[Res](f: Ref[scala.Function1[Costed[Item], Costed[Res]]]): Ref[CostedColl[Res]] = delayInvoke;
      @NeverInline def filterCosted(f: Ref[scala.Function1[Costed[Item], Costed[Boolean]]]): Ref[CostedColl[Item]] = delayInvoke;
      @NeverInline def foldCosted[B](zero: Ref[Costed[B]], op: Ref[scala.Function1[Costed[scala.Tuple2[B, Item]], Costed[B]]]): Ref[Costed[B]] = delayInvoke
    };
    abstract class CCostedBuilder extends CostedBuilder {
      def monoidBuilder: Ref[MonoidBuilder] = RMonoidBuilderInst();
      @NeverInline def costedValue[T](x: Ref[T], optCost: Ref[WOption[Int]]): Ref[Costed[T]] = delayInvoke;
      @NeverInline def defaultValue[T](valueType: Ref[WRType[T]]): Ref[T] = delayInvoke;
      def mkSizePrim[T](dataSize: Ref[Long], tT: Ref[WRType[T]]): Ref[SizePrim[T]] = RCSizePrim(dataSize, tT);
      def mkSizePair[L, R](l: Ref[Size[L]], r: Ref[Size[R]]): Ref[SizePair[L, R]] = RCSizePair(l, r);
      def mkSizeColl[T](sizes: Ref[Coll[Size[T]]]): Ref[SizeColl[T]] = RCSizeColl(sizes);
      def mkSizeFunc[E, A, R](sizeEnv: Ref[Size[E]], sizeFunc: Ref[Long], tA: Ref[WRType[A]], tR: Ref[WRType[R]]): Ref[SizeFunc[E, A, R]] = RCSizeFunc(sizeEnv, sizeFunc, tA, tR);
      def mkSizeOption[T](sizeOpt: Ref[WOption[Size[T]]]): Ref[SizeOption[T]] = RCSizeOption(sizeOpt);
      def mkCostedPrim[T](value: Ref[T], cost: Ref[Int], size: Ref[Size[T]]): Ref[CostedPrim[T]] = RCCostedPrim(value, cost, size);
      def mkCostedPair[L, R](first: Ref[Costed[L]], second: Ref[Costed[R]], accCost: Ref[Int]): Ref[CostedPair[L, R]] = RCCostedPair(first, second, accCost);
      def mkCostedFunc[Env, Arg, Res](envCosted: Ref[Costed[Env]], func: Ref[scala.Function1[Costed[Arg], Costed[Res]]], cost: Ref[Int], size: Ref[Size[scala.Function1[Arg, Res]]]): Ref[CostedFunc[Env, Arg, Res]] = RCCostedFunc(envCosted, func, cost, size);
      def mkCostedColl[T](values: Ref[Coll[T]], costs: Ref[Coll[Int]], sizes: Ref[Coll[Size[T]]], valuesCost: Ref[Int]): Ref[CostedColl[T]] = RCCostedColl(values, costs, sizes, valuesCost);
      def mkCostedOption[T](value: Ref[WOption[T]], costOpt: Ref[WOption[Int]], sizeOpt: Ref[WOption[Size[T]]], accumulatedCost: Ref[Int]): Ref[CostedOption[T]] = RCCostedOption(value, costOpt, sizeOpt, accumulatedCost)
    };
    trait CCostedPrimCompanion;
    trait CCostedPairCompanion;
    trait CCostedFuncCompanion;
    trait CCostedCollCompanion;
    trait CCostedBuilderCompanion
  }
}