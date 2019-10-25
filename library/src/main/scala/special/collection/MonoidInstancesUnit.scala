package special.collection {
  import scalan._

  trait MonoidInstances extends Base { self: Library =>
    import IntPlusMonoid._;
    import LongPlusMonoid._;
    import Monoid._;
    import MonoidBuilder._;
    abstract class MonoidBuilderInst extends MonoidBuilder {
      val `intPlusMonoid ` : Ref[IntPlusMonoid] = RIntPlusMonoid(toRep(0.asInstanceOf[Int]));
      def intPlusMonoid: Ref[IntPlusMonoid] = MonoidBuilderInst.this.`intPlusMonoid `;
      val `longPlusMonoid ` : Ref[LongPlusMonoid] = RLongPlusMonoid(toRep(0L.asInstanceOf[Long]));
      def longPlusMonoid: Ref[LongPlusMonoid] = MonoidBuilderInst.this.`longPlusMonoid `;
    };
    abstract class IntPlusMonoid(val zero: Ref[Int]) extends Monoid[Int] {
      def plus(x: Ref[Int], y: Ref[Int]): Ref[Int] = x.+(y);
      def power(x: Ref[Int], n: Ref[Int]): Ref[Int] = x.*(n)
    };
    abstract class LongPlusMonoid(val zero: Ref[Long]) extends Monoid[Long] {
      def plus(x: Ref[Long], y: Ref[Long]): Ref[Long] = x.+(y);
      def power(x: Ref[Long], n: Ref[Int]): Ref[Long] = x.*(n.toLong)
    };
    trait MonoidBuilderInstCompanion;
    trait IntPlusMonoidCompanion;
    trait LongPlusMonoidCompanion;
  }
}