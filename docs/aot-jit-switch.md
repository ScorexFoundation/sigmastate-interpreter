## A protocol for AOT -> JIT switch via soft-fork  

### Definitions
Term         | Description
-------------|------------ 
 _ScriptV1_  | The current version of ErgoTree (3.x releases) used in ErgoBlock v1. Bits 0-2 == 0 in the ErgoTree header byte. (see ErgoTree class).
 _ScriptV2_  | The next version of ErgoTree (5.x releases) used after SF is activated. Bits 0-2 == 1 in the ErgoTree header byte. (see ErgoTree class).
 R4.0-AOT-cost | cost estimation using v4.0 Ahead-Of-Time costing implementation
 R4.0-AOT-verify | spending condition verification using v4.0 Ahead-Of-Time interpreter implementation
 skip-pool-tx    | skip pool transaction when building a new block candidate 
 skip-accept     | skip script evaluation (both costing and verification) and treat it as True proposition (accept spending) 
 skip-reject     | skip script evaluation (both costing and verification) and treat it as False proposition (reject spending) 
 accept-overcosted | skip script verification and treat it as True proposition (accept spending) if cost is too high.
 validation state | a tuple of (`Block Type`, `SF Status`, `Script Version`)
 SF Status       | soft-fork status of the block

### Script Validation Rules Summary

Validation of scripts in blocks is defined for each release and depend on validation
context which includes type of block, SF status and script version. We denote blocks being
created by miners as `candidate` and those distributed across network as `mined`.

Thus, we have 8 different validation contexts multiplied by 2 node versions
having in total 16 validation rules as summarized in the following table.

Rule#| SF Status| Block Type| Script Version | Release | Validation Action 
-----|----------|-----------|----------------|---------|--------
1    | inactive | candidate | Script v1 | v4.0 | R4.0-AOT-cost, R4.0-AOT-verify
2    | inactive | candidate | Script v1 | v5.0 | R4.0-AOT-cost, R4.0-AOT-verify
3    | inactive | candidate | Script v2 | v4.0 | skip-pool-tx (cannot handle)
4    | inactive | candidate | Script v2 | v5.0 | skip-pool-tx (wait activation)
||||
5    | inactive | mined     | Script v1 | v4.0 | R4.0-AOT-cost, R4.0-AOT-verify 
6    | inactive | mined     | Script v1 | v5.0 | R4.0-AOT-cost, R4.0-AOT-verify 
7    | inactive | mined     | Script v2 | v4.0 | skip-reject (cannot handle)
8    | inactive | mined     | Script v2 | v5.0 | skip-reject (wait activation)
||||
9    | active   | candidate | Script v1 | v4.0 | R4.0-AOT-cost, R4.0-AOT-verify
10   | active   | candidate | Script v1 | v5.0 | R5.0-JIT-cost, R5.0-JIT-verify 
11   | active   | candidate | Script v2 | v4.0 | skip-pool-tx (cannot handle)
12   | active   | candidate | Script v2 | v5.0 | R5.0-JIT-cost, R5.0-JIT-verify 
||||
13   | active   | mined     | Script v1 | v4.0 | R4.0-AOT-verify, accept-overcosted (rely on majority)
14   | active   | mined     | Script v1 | v5.0 | R5.0-JIT-cost, R5.0-JIT-verify 
15   | active   | mined     | Script v2 | v4.0 | skip-accept (rely on majority)
16   | active   | mined     | Script v2 | v5.0 | R5.0-JIT-cost, R5.0-JIT-verify 

Observe the following properties of the validation rules.
1. Rules 1-4 mean the behaviour of v4.0 and v5.0 nodes is identical while creating new
block candidates before soft-fork.

2. For any given tuple (`SF`, `Script Version`, `Release`) the same `ValidationAction` is
applied for both `candidate` and `mined` blocks. This proves the consistency of the rules
with respect to the change of the block status from `candidate` to `mined`.

2. Each rule `i`, where `i` is an odd number, defines a `Validation Action` performed by
v4.0 nodes. Paired with it the `i+1` rule defines `Validation Action` performed by the
v5.0 nodes. Any such a pair `(i, i+1)` of rules have `Validation Actions` which are either
the same or equivalent with respect to the [Required properties of AOT and JIT
implementations](#required-properties-of-aot-and-jit-implementations). This proves
consistency of the validation actions across v4.0 and v5.0 nodes.

3. After SF is activated (`SF Status == active`), both AOT-cost and AOT-verify
implementations are no longer used in `Validation Action`. This allow us to remove
AOT implementation in v5.0.1. To do that, we rely on Prop3 and update validation rules for
`mined` blocks by replacing rules 10, 12, 14, 16 with 17, 18, 19, 20 respectively.  
See [the description of Rule 17](#rule-17) for details.

Rule#| SF Status| Block Type| Script Version | Release | Validation Action 
-----|-----------|----------|----------------|---------|--------
17    | inactive  | mined    | Script v1      | v5.0.1  | R5.0-JIT-cost, R5.0-JIT-verify 

### Rule Descriptions 

#### Rules 1 and 2
 _Handle v1 script in candidate block when SF is not active._
 Ensured by _Prop1_ and _Prop2_ both v4.0 and v5.0 nodes use equivalent AOT-cost and
 AOT-verify and thus have consensus.
 Release v5.0 will contain both AOT and JIT versions simultaneously and thus can _behave as v4.0_
 before SF is activated and _behave as v5.0_ after SF activation.
 
#### Rules 3 and 4
_Both v4.0 and v5.0 nodes reject v2 scripts in new candidate blocks when SF is not active._ 
This is ensured by _Prop5_ which is motivated by the following reasons: 
- v4.0 nodes have no idea of v2 scripts, thus rejecting them altogether both in input and
  output boxes of new candidate blocks
- v5.0 nodes are waiting for majority of nodes to vote, thus rejecting until SF is activated

#### Rules 5 and 6
These rules allow v1 scripts to enter blockchain even after SF is activated (for backward
compatibility with Apps).
In this case v4.0 node will _skip-accept_ v1 scripts in candidate blocks relying on the
majority. 
Now, after SF is activated, the majority consist of v5.0 nodes and they will do
`R5.0-JIT-verify(Script v1)` which is equivalent to `R4.0-AOT-verify(Script v1)` due to 
_Prop3_.
 
One remark. A specific version of ErgoTree (in this case v1) assumes the fixed semantics of
all operations which doesn't depend on the interpreter implementation and we use this fact
to switch from `R4.0-AOT-verify` to `R5.0-JIT-verify` based implementation.

However, for backward compatibility with Apps we DON'T NEED equivalence of costing, hence 
exact cost estimation is not necessary. For this reason we have the relaxed condition in 
_Prop4_, which means that any ScriptV1 admitted by `R4.0-AOT-cost` will also be admitted by
`R5.0-JIT-cost`.

#### Rules 7 and 8
After SF is activated v4.0 node `skip-accept` by relying on the majority of v5.0
nodes, and also since it cannot handle v2 scripts. The majority of nodes uses new JIT
based implementation of ErgoTree interpreter: `R5.0-JIT-cost` procedure for costing and
`R5.0-JIT-verify` for verification.

#### Rule 17
The idea is to use `skip costing` action in Rule 17 which is applied to the old historical
blocks before the SF is activated. We assume it is safe to do for the following reasons:
  - v5.0.1 node assumes the SF has already been activated so `inactive` block is somewhere
  in the history.


### Required properties of AOT and JIT implementations

_Prop 1._ AOT-verify is preserved: `forall s:ScriptV1, R4.0-AOT-verify(s) == R5.0-AOT-verify(s)` 

_Prop 2._ AOT-cost is preserved: `forall s:ScriptV1, R4.0-AOT-cost(s) == R5.0-AOT-cost(s)`

_Prop 3._ JIT-verify can replace AOT-verify: `forall s:ScriptV1, R5.0-JIT-verify(s) == R4.0-AOT-verify(s)`

_Prop 4._ JIT-cost is bound by AOT-cost: `forall s:ScriptV1, R5.0-JIT-cost(s) <= R4.0-AOT-cost(s)`

_Prop 5._ ScriptV2 is rejected before SF is active:  
`forall s:ScriptV2, if not SF is active => R4.0-verify(s) == R5.0-verify(s) == Reject`

### Rules

1)  We will increment script from v1 to v2 by changing _version bits_ in ErgoTree header
(now they have value 0, and will be 1).

2) All the nodes should collectively (based on voting data in the blockchain) to start producing
v2 blocks and rejecting v1 block candidates (while accepting mined historical v1 blocks)

2) The 4.0 node, after soft-fork 5.0 is activated will check the version, and if it is > 0
it will accept the script without validation (at this point the majority of nodes will be
5.0)

3) after soft-fork 5.0 is activated, all 5.0 nodes will execute v0 scripts using current
AOT based interpreter, and v1 script using JIT based interpreter

4) before soft-fork is activated all nodes (4.0 and 5.0 releases) will execute only v0
scripts using AOT based interpreter and will reject v1 scripts

5) both v0 and v1 scripts will be supported by the network and applications may create
transactions with both v0 and v1 scripts.

6) Since we are going to fix some bugs, the behaviour of v0 and v1 scripts in general not
required to be precisely equivalent. So while old apps are supported unchanged, new apps
are encouraged to use new ErgoScript frontend which will compile v1 scripts

### Specification
#### Notes
- Also, on v0 and v2, it would be better to avoid changing semantics of existing ops,
deprecation old and introducing new ones is cleaner