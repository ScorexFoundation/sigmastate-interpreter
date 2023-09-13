package sigma.js

import sigma.{Coll, Colls}
import sigma.data.{Iso, RType}

import scala.reflect.ClassTag
import scala.scalajs.js
import scala.scalajs.js.JSConverters.JSRichOption

object Isos {

  implicit def isoUndefOr[A, B](implicit iso: Iso[A, B]): Iso[js.UndefOr[A], Option[B]] = new Iso[js.UndefOr[A], Option[B]] {
    override def to(x: js.UndefOr[A]): Option[B] = x.toOption.map(iso.to)
    override def from(x: Option[B]): js.UndefOr[A] = x.map(iso.from).orUndefined
  }

  implicit def isoArrayToColl[A, B](iso: Iso[A, B])
      (implicit ctA: ClassTag[A], tB: RType[B]): Iso[js.Array[A], Coll[B]] = new Iso[js.Array[A], Coll[B]] {
    override def to(x: js.Array[A]): Coll[B] = Colls.fromArray(x.map(iso.to).toArray(tB.classTag))
    override def from(x: Coll[B]): js.Array[A] = js.Array(x.toArray.map(iso.from): _*)
  }

  implicit def isoArrayToIndexed[A, B](iso: Iso[A, B])
      (implicit cB: ClassTag[B]): Iso[js.Array[A], IndexedSeq[B]] = new Iso[js.Array[A], IndexedSeq[B]] {
    override def to(x: js.Array[A]): IndexedSeq[B] = x.map(iso.to).toArray(cB).toIndexedSeq
    override def from(x: IndexedSeq[B]): js.Array[A] = js.Array(x.map(iso.from): _*)
  }

}
