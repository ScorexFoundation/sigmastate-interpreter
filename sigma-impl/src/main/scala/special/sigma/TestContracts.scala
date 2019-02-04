package special.sigma

class CrowdFundingContract(
    val deadline: Long, val minToRaise: Long,
    val backerPubKey: SigmaProp,
    val projectPubKey: SigmaProp
) extends CrowdFunding with DefaultContract {
  override def builder: SigmaDslBuilder = new TestSigmaDslBuilder
}

class DemurrageCurrencyContract(
    val demurragePeriod: Int,
    val demurrageCost: Long,
    val regScript: SigmaProp
) extends DemurrageCurrency with DefaultContract {
  override def builder: SigmaDslBuilder = new TestSigmaDslBuilder
}

