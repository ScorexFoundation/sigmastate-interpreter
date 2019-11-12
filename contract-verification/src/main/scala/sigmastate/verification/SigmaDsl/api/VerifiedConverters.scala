package sigmastate.verification.SigmaDsl.api

import sigmastate.basics.DLogProtocol.ProveDlog
import sigmastate.eval.CSigmaProp
import sigmastate.verification.SigmaDsl.api.collection.{Coll, CollProof}
import sigmastate.verification.SigmaDsl.api.sigma.{ProveDlogProof, SigmaProp, SigmaPropProof}
import special.collection.CollOverArray
import stainless.annotation.ignore

import scala.reflect.ClassTag
import scala.language.implicitConversions

@ignore
object VerifiedConverters {

  implicit def verifiedCollToColl[A: ClassTag](coll: Coll[A]): special.collection.Coll[A] = {
    coll match {
      case CollProof(xs) =>
        import scala.collection.mutable
        val tA = scalan.RType.fromClassTag[A](implicitly[ClassTag[A]])
        // mutable.ArrayBuilder.make crashes the scalac
        val ll = mutable.MutableList[A]()
        val xsSize = xs.length.toInt
        for(i <- 0 until xsSize) {
          ll += xs(i)
        }
        new CollOverArray[A](ll.toArray)(tA)
    }
  }

  implicit def verifiedSigmaPropToSigmaProp(prop: SigmaProp): special.sigma.SigmaProp = prop match {
    case SigmaPropProof(ProveDlogProof(value)) => new CSigmaProp(ProveDlog(value))
  }
}
