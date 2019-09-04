package special.collection {
  import scalan._

  trait CostedOptions extends Base { self: Library =>
    import CCostedBuilder._;
    import CostedBuilder._;
    import CostedOption._;
    import Size._;
    import SizeOption._;
    import WOption._;
    abstract class CCostedOption[T](val value: Ref[WOption[T]], val costOpt: Ref[WOption[Int]], val sizeOpt: Ref[WOption[Size[T]]], val accumulatedCost: Ref[Int]) extends CostedOption[T] {
      def builder: Ref[CostedBuilder] = RCCostedBuilder();
      @NeverInline def cost: Ref[Int] = delayInvoke;
      def size: Ref[Size[WOption[T]]] = CCostedOption.this.builder.mkSizeOption[T](CCostedOption.this.sizeOpt)
    };
    trait CCostedOptionCompanion
  }
}