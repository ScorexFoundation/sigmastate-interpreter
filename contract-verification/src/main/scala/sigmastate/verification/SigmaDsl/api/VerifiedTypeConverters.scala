package sigmastate.verification.SigmaDsl.api

import sigmastate.Values.{CollectionConstant, ConcreteCollection, Constant, EvaluatedValue}
import sigmastate.basics.DLogProtocol.ProveDlog
import sigmastate.eval.Evaluation.rtypeToSType
import sigmastate.eval.{CSigmaProp, Evaluation}
import sigmastate.verification.SigmaDsl.api.collection.Coll
import sigmastate.verification.SigmaDsl.api.sigma.{ProveDlogProof, SigmaProp, SigmaPropProof}
import sigmastate.{SCollection, SCollectionType, SType}
import special.collection.CollOverArrayBuilder
import stainless.annotation.ignore

import scala.language.{higherKinds, implicitConversions}

@ignore
object VerifiedTypeConverters {

  implicit def VRTypeToRType[A, B]: Iso[RType[A], scalan.RType[B]] =
    new Iso[RType[A], scalan.RType[B]] {
      override def to(as: RType[A]): scalan.RType[B] = (as match {
        case RType.PrimitiveType(classTag) => scalan.RType.fromClassTag(classTag)
        case RType.CollType(tA) => special.collection.CollType(VRTypeToRType.to(tA))
        case RType.PairType(tFst, tSnd) => scalan.RType.PairType(VRTypeToRType.to(tFst), VRTypeToRType.to(tSnd))
      }).asInstanceOf[scalan.RType[B]]

      override def from(b: scalan.RType[B]): RType[A] = ???
    }

  implicit def vRTypeToRType[A, B](as: RType[A]): scalan.RType[B] = VRTypeToRType.to(as)

  implicit def VCollToColl[A, B](implicit itemIso: Iso[A, B]): Iso[Coll[A], special.collection.Coll[B]] =
    new Iso[Coll[A], special.collection.Coll[B]] {
      override def to(a: Coll[A]): special.collection.Coll[B] = {
        val rtB: scalan.RType[B] = a.tItem
        val arrB = a.toArray.map(itemIso.to).toArray[B](rtB.classTag)
        (new CollOverArrayBuilder).fromArray(arrB)(rtB)
      }
      // TODO: implement and use in tests
      override def from(b: special.collection.Coll[B]): Coll[A] = ???
    }

  implicit def Tuple2ToTuple2[A1, A2, B1, B2](implicit itemIso1: Iso[A1, A2], itemIso2: Iso[B1, B2]): Iso[(A1, B1), (A2, B2)] =
    new Iso[(A1, B1), (A2, B2)] {
      override def to(a: (A1, B1)): (A2, B2) = (itemIso1.to(a._1), itemIso2.to(a._2))
      override def from(b: (A2, B2)): (A1, B1) = (itemIso1.from(b._1), itemIso2.from(b._2))
    }

  implicit val VSigmaPropToSigmaProp: Iso[SigmaProp, special.sigma.SigmaProp] =
    new Iso[SigmaProp, special.sigma.SigmaProp] {
      override def to(a: SigmaProp): special.sigma.SigmaProp = a match {
        case SigmaPropProof(ProveDlogProof(v)) => new CSigmaProp(ProveDlog(v))
      }

      override def from(b: special.sigma.SigmaProp): SigmaProp = b match {
        case CSigmaProp(ProveDlog(v)) => SigmaPropProof(ProveDlogProof(v))
      }
    }

  def VCollToErgoTree[A]: Iso[Coll[A], EvaluatedValue[SCollection[SType]]] =
    new Iso[Coll[A], EvaluatedValue[SCollection[SType]]] {

      override def to(a: Coll[A]): EvaluatedValue[SCollection[SType]] = {
        a.tItem match {
          case RType.CollType(tA) =>
            val c = a.asInstanceOf[Coll[Coll[Any]]].toArray.map { x => VCollToErgoTree.to(x) }
            ConcreteCollection(c, SCollectionType(rtypeToSType(tA)))
          case tA@RType.PrimitiveType(_) =>
            val st = Evaluation.rtypeToSType(tA)
            val innerColl = (new CollOverArrayBuilder).fromArray(a.toArray)(tA)
              .asInstanceOf[special.collection.Coll[st.type#WrappedType]]
            CollectionConstant[st.type](innerColl, st).asInstanceOf[Constant[SCollection[SType]]]
        }
      }

      override def from(b: EvaluatedValue[SCollection[SType]]): Coll[A] = ???
    }
}
