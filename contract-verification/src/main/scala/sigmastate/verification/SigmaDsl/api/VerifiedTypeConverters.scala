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
import scala.reflect.ClassTag

@ignore
object VerifiedTypeConverters {

  implicit def verifiedCollToColl[A, B](coll: Coll[A])(implicit convert: A => B): special.collection.Coll[B] = {
    val arr = coll.toArray.map(convert)
    val ctB: ClassTag[B] = (coll.tItem match {
      case RType.CollType(tA) => scala.reflect.classTag[special.collection.Coll[B]]
      case RType.PrimitiveType(classTag) => classTag
    }).asInstanceOf[ClassTag[B]]
    val rtB: scalan.RType[B] = scalan.RType.fromClassTag(ctB)
    (new CollOverArrayBuilder).fromArray(arr.toArray[B](rtB.classTag))(rtB)
  }

  implicit def verifiedSigmaPropToSigmaProp(prop: SigmaProp): special.sigma.SigmaProp = prop match {
    case SigmaPropProof(ProveDlogProof(value)) => new CSigmaProp(ProveDlog(value))
  }

  implicit def verifiedRTypeToRType[A](tA: RType[A]): scalan.RType[A] = scalan.RType.fromClassTag(tA.classTag)

  def verifiedCollToTree[A](coll: Coll[A]): EvaluatedValue[SCollection[SType]] = coll.tItem match {
    case RType.CollType(tA) =>
      val c = coll.asInstanceOf[Coll[Coll[Any]]].toArray.map { x => verifiedCollToTree(x) }
      ConcreteCollection(c, SCollectionType(rtypeToSType(tA)))
    case tA @ RType.PrimitiveType(_) =>
      val st = Evaluation.rtypeToSType(tA)
      val innerColl = (new CollOverArrayBuilder).fromArray(coll.toArray)(tA)
        .asInstanceOf[special.collection.Coll[st.type#WrappedType]]
      CollectionConstant[st.type](innerColl, st).asInstanceOf[Constant[SCollection[SType]]]
  }
}
