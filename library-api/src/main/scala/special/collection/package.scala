package special

import scalan.RType
import scala.reflect.{ClassTag, classTag}

package collection {
  case class CollType[A](tItem: RType[A]) extends RType[Coll[A]] {
    val classTag: ClassTag[Coll[A]] = ClassTag[Coll[A]](classOf[Coll[A]])
    override def name: String = s"Coll[${tItem.name}]"
    override def isConstantSize: Boolean = false
  }
  case class ReplCollType[A](tItem: RType[A]) extends RType[ReplColl[A]] {
    val classTag: ClassTag[ReplColl[A]] = ClassTag[ReplColl[A]](classOf[ReplColl[A]])
    override def name: String = s"ReplColl[${tItem.name}]"
    override def isConstantSize: Boolean = false
  }

  case class SizeType[A](tVal: RType[A]) extends RType[Size[A]] {
    val classTag: ClassTag[Size[A]] = ClassTag[Size[A]](classOf[Size[A]])
    override def name: String = s"Size[${tVal.name}]"
    override def isConstantSize: Boolean = tVal.isConstantSize
  }
  case class SizePrimType[A](tVal: RType[A]) extends RType[SizePrim[A]] {
    val classTag: ClassTag[SizePrim[A]] = ClassTag[SizePrim[A]](classOf[SizePrim[A]])
    override def name: String = s"SizePrim[${tVal.name}]"
    override def isConstantSize: Boolean = tVal.isConstantSize
  }
  case class SizePairType[A,B](tFst: RType[A], tSnd: RType[B]) extends RType[SizePair[A, B]] {
    val classTag: ClassTag[SizePair[A, B]] = ClassTag[SizePair[A, B]](classOf[SizePair[A, B]])
    override def name: String = s"SizePair[${tFst.name},${tSnd.name}]"
    override def isConstantSize: Boolean = tFst.isConstantSize && tSnd.isConstantSize
  }
  case class SizeCollType[A](tItem: RType[A]) extends RType[SizeColl[A]] {
    val classTag: ClassTag[SizeColl[A]] = ClassTag[SizeColl[A]](classOf[SizeColl[A]])
    override def name: String = s"SizeColl[${tItem.name}]"
    override def isConstantSize: Boolean = tItem.isConstantSize
  }
  case class SizeFuncType[E,A,B](tEnv: RType[E], tDom: RType[A], tRange: RType[B]) extends RType[SizeFunc[E, A, B]] {
    val classTag: ClassTag[SizeFunc[E, A, B]] = ClassTag[SizeFunc[E, A, B]](classOf[SizeFunc[E, A, B]])
    override def name: String = s"SizeFunc[${tEnv.name},${tDom.name},${tRange.name}]"
    override def isConstantSize: Boolean = false
  }
  case class SizeOptionType[A](tItem: RType[A]) extends RType[SizeOption[A]] {
    val classTag: ClassTag[SizeOption[A]] = ClassTag[SizeOption[A]](classOf[SizeOption[A]])
    override def name: String = s"SizeOption[${tItem.name}]"
    override def isConstantSize: Boolean = tItem.isConstantSize
  }
}

package object collection {
  implicit def collRType[A](implicit tA: RType[A]): RType[Coll[A]] = CollType[A](tA)
  implicit def extendCollType[A](ct: RType[Coll[A]]): CollType[A] = ct.asInstanceOf[CollType[A]]

  implicit def replCollRType[A](implicit tA: RType[A]): RType[ReplColl[A]] = ReplCollType[A](tA)

  implicit val collBuilderRType: RType[CollBuilder] = RType.fromClassTag(classTag[CollBuilder])

  implicit def sizeRType[A](implicit tA: RType[A]): RType[Size[A]] = SizeType[A](tA)
  implicit def extendSizeType[A](ct: RType[Size[A]]): SizeType[A] = ct.asInstanceOf[SizeType[A]]

  implicit def sizePrimRType[A](implicit tA: RType[A]): RType[SizePrim[A]] = SizePrimType[A](tA)
  implicit def extendSizePrimType[A](ct: RType[SizePrim[A]]): SizePrimType[A] = ct.asInstanceOf[SizePrimType[A]]
  
  implicit def sizePairRType[A, B](implicit tA: RType[A], tB: RType[B]): RType[SizePair[A, B]] = SizePairType[A, B](tA, tB)
  implicit def extendSizePairType[A, B](ct: RType[SizePair[A,B]]): SizePairType[A, B] = ct.asInstanceOf[SizePairType[A, B]]

  implicit def sizeCollRType[A](implicit tA: RType[A]): RType[SizeColl[A]] = SizeCollType[A](tA)
  implicit def extendSizeCollType[A](ct: RType[SizeColl[A]]): SizeCollType[A] = ct.asInstanceOf[SizeCollType[A]]

  implicit def sizeFuncType[E, A, B](implicit tE: RType[E], tA: RType[A], tB: RType[B]): RType[SizeFunc[E, A, B]] = SizeFuncType[E, A, B](tE, tA, tB)
  implicit def extendSizeFuncType[E, A, B](ct: RType[SizeFunc[E,A,B]]): SizeFuncType[E, A, B] = ct.asInstanceOf[SizeFuncType[E, A, B]]
  
  implicit def sizeOptionRType[A](implicit tA: RType[A]): RType[SizeOption[A]] = SizeOptionType[A](tA)
  implicit def extendSizeOptionType[A](ct: RType[SizeOption[A]]): SizeOptionType[A] = ct.asInstanceOf[SizeOptionType[A]]

}
