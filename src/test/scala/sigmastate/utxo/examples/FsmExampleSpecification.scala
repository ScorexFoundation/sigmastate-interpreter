package sigmastate.utxo.examples

import sigmastate.helpers.SigmaTestingCommons

/**
  * Similarly to MAST example, we can do more complex contracts, e.g. ones with cycles. For example, we can do a
  * contract described as a finite state machine.
  * Let's consider that a input in a finite state machine is its script, or its hash. Then assume a machine is
  * described as a table of transitions, an example is below:
  * state1 | script_hash1 | state2
  * state2 | script_hash2 | state3
  * state2 | script_hash3 | state1
  * state3 | script_hash4 | F
  *
  * where F is a final state, which denotes end of a contract execution
  */

class FsmExampleSpecification extends SigmaTestingCommons {

}
