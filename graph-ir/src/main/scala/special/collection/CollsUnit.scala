package special.collection {
  import scalan._

  trait Colls extends Base { self: Library =>
    import Coll._;
    import CollBuilder._;
    import WOption._;
    trait Coll[A] extends Def[Coll[A]] {
      implicit def eA: Elem[A];
      def builder: Ref[CollBuilder];
      def length: Ref[Int];
      def size: Ref[Int] = this.length;
      def isEmpty: Ref[Boolean];
      def nonEmpty: Ref[Boolean];
      def apply(i: Ref[Int]): Ref[A];
      def isDefinedAt(idx: Ref[Int]): Ref[Boolean];
      def getOrElse(index: Ref[Int], default: Ref[A]): Ref[A];
      def map[B](f: Ref[scala.Function1[A, B]]): Ref[Coll[B]];
      def zip[B](ys: Ref[Coll[B]]): Ref[Coll[scala.Tuple2[A, B]]];
      def exists(p: Ref[scala.Function1[A, Boolean]]): Ref[Boolean];
      def forall(p: Ref[scala.Function1[A, Boolean]]): Ref[Boolean];
      def filter(p: Ref[scala.Function1[A, Boolean]]): Ref[Coll[A]];
      def foldLeft[B](zero: Ref[B], op: Ref[scala.Function1[scala.Tuple2[B, A], B]]): Ref[B];
      def indices: Ref[Coll[Int]];
      def flatMap[B](f: Ref[scala.Function1[A, Coll[B]]]): Ref[Coll[B]];
      def segmentLength(p: Ref[scala.Function1[A, Boolean]], from: Ref[Int]): Ref[Int];
      def find(p: Ref[scala.Function1[A, Boolean]]): Ref[WOption[A]] = delayInvoke;
      def indexWhere(p: Ref[scala.Function1[A, Boolean]], from: Ref[Int]): Ref[Int];
      def indexOf(elem: Ref[A], from: Ref[Int]): Ref[Int] = delayInvoke;
      def lastIndexWhere(p: Ref[scala.Function1[A, Boolean]], end: Ref[Int]): Ref[Int];
      def take(n: Ref[Int]): Ref[Coll[A]];
      def patch(from: Ref[Int], patch: Ref[Coll[A]], replaced: Ref[Int]): Ref[Coll[A]];
      def updated(index: Ref[Int], elem: Ref[A]): Ref[Coll[A]];
      def updateMany(indexes: Ref[Coll[Int]], values: Ref[Coll[A]]): Ref[Coll[A]];
      def unionSet(that: Ref[Coll[A]]): Ref[Coll[A]];
      def diff(that: Ref[Coll[A]]): Ref[Coll[A]] = delayInvoke;
      def intersect(that: Ref[Coll[A]]): Ref[Coll[A]] = delayInvoke;
      def slice(from: Ref[Int], until: Ref[Int]): Ref[Coll[A]];
      def append(other: Ref[Coll[A]]): Ref[Coll[A]];
      def reverse: Ref[Coll[A]]
    };
    trait CollBuilder extends Def[CollBuilder] {
      def fromItems[T](items: Ref[T]*)(implicit cT: Elem[T]): Ref[Coll[T]];
      def xor(left: Ref[Coll[Byte]], right: Ref[Coll[Byte]]): Ref[Coll[Byte]];
      def replicate[T](n: Ref[Int], v: Ref[T]): Ref[Coll[T]];
    };
    trait CollCompanion;
    trait CollBuilderCompanion
  }
}