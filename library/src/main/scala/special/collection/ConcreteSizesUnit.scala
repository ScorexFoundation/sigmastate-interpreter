package special.collection {
  import scalan._

  trait ConcreteSizes extends Base { self: Library =>
    import SizeColl._;
    import SizeFunc._;
    import SizeOption._;
    import SizePair._;
    import SizePrim._;
    abstract class CSizePrim[Val](val dataSize: Ref[Long], val tVal: Ref[WRType[Val]]) extends SizePrim[Val];
    abstract class CSizePair[L, R](val l: Ref[Size[L]], val r: Ref[Size[R]]) extends SizePair[L, R] {
      @NeverInline def dataSize: Ref[Long] = delayInvoke
    };
    abstract class CSizeColl[Item](val sizes: Ref[Coll[Size[Item]]]) extends SizeColl[Item] {
      @NeverInline def dataSize: Ref[Long] = delayInvoke
    };
    abstract class CSizeFunc[Env, Arg, Res](val sizeEnv: Ref[Size[Env]], val sizeFunc: Ref[Long], val tArg: Ref[WRType[Arg]], val tRes: Ref[WRType[Res]]) extends SizeFunc[Env, Arg, Res] {
      @NeverInline def dataSize: Ref[Long] = delayInvoke
    };
    abstract class CSizeOption[Item](val sizeOpt: Ref[WOption[Size[Item]]]) extends SizeOption[Item] {
      @NeverInline def dataSize: Ref[Long] = delayInvoke
    };
    trait CSizePrimCompanion;
    trait CSizePairCompanion;
    trait CSizeCollCompanion;
    trait CSizeFuncCompanion;
    trait CSizeOptionCompanion
  }
}