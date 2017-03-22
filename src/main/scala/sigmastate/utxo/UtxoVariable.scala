package sigmastate.utxo

import sigmastate._
import org.bitbucket.inkytonik.kiama.rewriting.Rewriter.{rule, everywherebu, everywheretd}


trait UtxoVariable[V <: Value] extends Variable[V]

case object OutputAmount extends Variable[IntLeaf]

//information needed
trait ScopedBinding {
  //todo: more strict-type solution Variable[V] => Value[V]
  val bindings: Map[Variable[_], Value]

  val relations: Seq[Relation]
}

trait Function extends StateTree
case class TxHasOutput(relation: Relation*) extends Function

object UtxoSubstitution{
  /*
    val fnSubst = rule[StateTree]{
      case hasOut: TxHasOutput => hasOut.relation
    }
  */
}


