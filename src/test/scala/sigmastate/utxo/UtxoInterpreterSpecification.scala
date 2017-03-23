package sigmastate.utxo

import org.scalatest.{Matchers, PropSpec}
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}


class UtxoInterpreterSpecification extends PropSpec
  with PropertyChecks
  with GeneratorDrivenPropertyChecks
  with Matchers {

  ignore("Evaluation - Crowdfunding Example") {
    // (height >= 100 /\ dlog_g x1) \/ (height < 100 /\ has_output(amount >= 100000, proposition = dlog_g x2)
  }

  ignore("Evaluation - Demurrage Example") {

  }
}
