package wrappers.scala {
  import scalan._

  import special.wrappers.WrappersModule

  trait WOptions extends Base { self: WrappersModule =>
    @External("Option") @ContainerType @FunctorType @Liftable @WithMethodCallRecognizers trait WOption[A] extends Def[WOption[A]] {
      implicit def eA: Elem[A];
      @External def fold[B](ifEmpty: Ref[Thunk[B]], f: Ref[scala.Function1[A, B]]): Ref[B];
      @External def isEmpty: Ref[Boolean];
      @External def isDefined: Ref[Boolean];
      @External def filter(p: Ref[scala.Function1[A, Boolean]]): Ref[WOption[A]];
      @External def flatMap[B](f: Ref[scala.Function1[A, WOption[B]]]): Ref[WOption[B]];
      @External def map[B](f: Ref[scala.Function1[A, B]]): Ref[WOption[B]];
      @External def getOrElse[B](default: Ref[Thunk[B]]): Ref[B];
      @External def get: Ref[A]
    };
    trait WOptionCompanion
  }
}