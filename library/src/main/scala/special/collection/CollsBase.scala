package special.collection {
  import scalan._

  trait Colls extends Base { self: Library =>
    import Coll._;
    import CollBuilder._;
    import Monoid._;
    import MonoidBuilder._;
    import PairColl._;
    import WOption._;
    @ContainerType @FunctorType @Liftable @WithMethodCallRecognizers trait Coll[A] extends Def[Coll[A]] {
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
      @NeverInline def find(p: Ref[scala.Function1[A, Boolean]]): Ref[WOption[A]] = delayInvoke;
      def indexWhere(p: Ref[scala.Function1[A, Boolean]], from: Ref[Int]): Ref[Int];
      @NeverInline def indexOf(elem: Ref[A], from: Ref[Int]): Ref[Int] = delayInvoke;
      def lastIndexWhere(p: Ref[scala.Function1[A, Boolean]], end: Ref[Int]): Ref[Int];
      def take(n: Ref[Int]): Ref[Coll[A]];
      def partition(pred: Ref[scala.Function1[A, Boolean]]): Ref[scala.Tuple2[Coll[A], Coll[A]]];
      def patch(from: Ref[Int], patch: Ref[Coll[A]], replaced: Ref[Int]): Ref[Coll[A]];
      def updated(index: Ref[Int], elem: Ref[A]): Ref[Coll[A]];
      def updateMany(indexes: Ref[Coll[Int]], values: Ref[Coll[A]]): Ref[Coll[A]];
      def mapReduce[K, V](m: Ref[scala.Function1[A, scala.Tuple2[K, V]]], r: Ref[scala.Function1[scala.Tuple2[V, V], V]]): Ref[Coll[scala.Tuple2[K, V]]];
      @NeverInline def groupBy[K](key: Ref[scala.Function1[A, K]]): Ref[Coll[scala.Tuple2[K, Coll[A]]]] = delayInvoke;
      @NeverInline def groupByProjecting[K, V](key: Ref[scala.Function1[A, K]], proj: Ref[scala.Function1[A, V]]): Ref[Coll[scala.Tuple2[K, Coll[V]]]] = delayInvoke;
      def unionSet(that: Ref[Coll[A]]): Ref[Coll[A]];
      @NeverInline def diff(that: Ref[Coll[A]]): Ref[Coll[A]] = delayInvoke;
      @NeverInline def intersect(that: Ref[Coll[A]]): Ref[Coll[A]] = delayInvoke;
      def sum(m: Ref[Monoid[A]]): Ref[A];
      def slice(from: Ref[Int], until: Ref[Int]): Ref[Coll[A]];
      def append(other: Ref[Coll[A]]): Ref[Coll[A]];
      def reverse: Ref[Coll[A]]
    };
    @WithMethodCallRecognizers trait PairColl[L, R] extends Coll[scala.Tuple2[L, R]] {
      implicit def eL: Elem[L];
      implicit def eR: Elem[R];
      def ls: Ref[Coll[L]];
      def rs: Ref[Coll[R]];
      def mapFirst[T1](f: Ref[scala.Function1[L, T1]]): Ref[Coll[scala.Tuple2[T1, R]]];
      def mapSecond[T1](f: Ref[scala.Function1[R, T1]]): Ref[Coll[scala.Tuple2[L, T1]]]
    };
    @Liftable @WithMethodCallRecognizers trait ReplColl[A] extends Coll[A] {
      implicit def eA: Elem[A];
      def value: Ref[A];
      def length: Ref[Int];
      def append(other: Ref[Coll[A]]): Ref[Coll[A]]
    };
    @Liftable @WithMethodCallRecognizers trait CollBuilder extends Def[CollBuilder] {
      def Monoids: Ref[MonoidBuilder];
      def pairColl[A, B](as: Ref[Coll[A]], bs: Ref[Coll[B]]): Ref[PairColl[A, B]];
      @Reified(value = "T") def fromItems[T](items: Ref[T]*)(implicit cT: Elem[T]): Ref[Coll[T]];
      def unzip[A, B](xs: Ref[Coll[scala.Tuple2[A, B]]]): Ref[scala.Tuple2[Coll[A], Coll[B]]];
      def xor(left: Ref[Coll[Byte]], right: Ref[Coll[Byte]]): Ref[Coll[Byte]];
      def replicate[T](n: Ref[Int], v: Ref[T]): Ref[Coll[T]];
      def emptyColl[T](implicit tT: Elem[T]): Ref[Coll[T]];
      def outerJoin[K, L, R, O](left: Ref[Coll[scala.Tuple2[K, L]]], right: Ref[Coll[scala.Tuple2[K, R]]])(l: Ref[scala.Function1[scala.Tuple2[K, L], O]], r: Ref[scala.Function1[scala.Tuple2[K, R], O]], inner: Ref[scala.Function1[scala.Tuple2[K, scala.Tuple2[L, R]], O]]): Ref[Coll[scala.Tuple2[K, O]]];
      def flattenColl[A](coll: Ref[Coll[Coll[A]]]): Ref[Coll[A]]
    };
    trait CollCompanion;
    trait PairCollCompanion;
    trait ReplCollCompanion;
    trait CollBuilderCompanion
  }
}