package sigmastate.utxo.examples

import special.sigma.{Context, Box}
import org.ergoplatform.dsl.{SigmaContractSyntax, ContractSpec, StdContracts}

class CrowdFunding[Spec <: ContractSpec]
    (val deadline: Int, val minToRaise: Long)
    (implicit val spec: Spec)
    extends SigmaContractSyntax with StdContracts
{
  import spec._
  /** The party, who wants to fund some amount of Ergs to the project. */
  val backer = ProvingParty("Backer")

  /** The party, who wants to collect at least `minToRaise` Ergs before `deadline`. */
  val project = ProvingParty("Project")

  val verifier = VerifyingParty("Miner")
  def pkBacker = backer.pubKey
  def pkProject = project.pubKey
  import syntax._
  lazy val env = Env("pkBacker" -> pkBacker, "pkProject" -> pkProject, "deadline" -> deadline, "minToRaise" -> minToRaise)

  lazy val holderProp = proposition("holder", { ctx: Context =>
    import ctx._
    val fundraisingFailure = HEIGHT >= deadline && pkBacker
    val enoughRaised = {(outBox: Box) =>
      outBox.value >= minToRaise &&
          outBox.propositionBytes == pkProject.propBytes
    }
    val fundraisingSuccess = HEIGHT < deadline &&
        pkProject &&
        OUTPUTS.exists(enoughRaised)

    fundraisingFailure || fundraisingSuccess
  },
  env,
  """
   |{
   |  val fundraisingFailure = HEIGHT >= deadline && pkBacker
   |    val enoughRaised = {(outBox: Box) =>
   |      outBox.value >= minToRaise &&
   |          outBox.propositionBytes == pkProject.propBytes
   |    }
   |    val fundraisingSuccess = HEIGHT < deadline &&
   |        pkProject &&
   |        OUTPUTS.exists(enoughRaised)
   |
   |    fundraisingFailure || fundraisingSuccess
   |}
  """.stripMargin)

  lazy val backerSignature  = proposition("backerSignature", _ => pkBacker, env, "pkBacker")
  lazy val projectSignature = proposition("projectSignature", _ => pkProject, env, "pkProject")

  lazy val oldProp = proposition("old", { ctx: Context =>
    import ctx._
    val c1 = HEIGHT >= deadline && pkBacker
    val c2 =
      HEIGHT < deadline && pkProject &&
      OUTPUTS.exists({ (out: Box) =>
        out.value >= minToRaise && out.propositionBytes == pkProject.propBytes
      })
    c1 || c2
  },
  env,
  """
   |{
   |  val c1 = HEIGHT >= deadline && pkBacker
   |  val c2 =
   |    HEIGHT < deadline && pkProject &&
   |    OUTPUTS.exists({ (out: Box) =>
   |      out.value >= minToRaise && out.propositionBytes == pkProject.propBytes
   |    })
   |  c1 || c2
   |}
  """.stripMargin)

}
