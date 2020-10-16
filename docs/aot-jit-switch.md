## The Protocol and Requirements to replace AOT with JIT via soft-fork  

The goal of this document is to specify requirements for v4.0, v5.0 and upcoming releases.
It also specifies rules of transaction validation in the Ergo network with respect of
soft-fork activation which should be followed by different versions of nodes. 
The v4.x -> v5.x soft-fork is motivated by the goal of switching from AOT to JIT-based
costing algorithm and the simplified ErgoTree interpreter.

### Definitions
The text below we use the terms defined in the following table, please refer to it when
necessary.

Term             | Description
-----------------|------------ 
 _ScriptV1_      | The current version of ErgoTree (3.x releases) used in ErgoBlock v1. Bits 0-2 == 0 in the ErgoTree header byte. (see ErgoTree class).
 _ScriptV2_      | The next version of ErgoTree (5.x releases) used after SF is activated. Bits 0-2 == 1 in the ErgoTree header byte. (see ErgoTree class).
 R4.0-AOT-cost   | cost estimation using v4.0 Ahead-Of-Time costing implementation
 R4.0-AOT-verify | spending condition verification using v4.0 Ahead-Of-Time interpreter implementation
 R5.0-JIT-verify | spending condition verification using v5.0 simplified interpreter with Just-In-Time costing of fullReduction and AOT sigma protocol costing.
 skip-pool-tx    | skip pool transaction when building a new block candidate 
 skip-accept     | skip script evaluation (both costing and verification) and treat it as True proposition (accept spending) 
 skip-reject     | skip script evaluation (both costing and verification) and treat it as False proposition (reject spending) 
 Validation Context | a tuple of (`Block Type`, `SF Status`, `Script Version`)
 SF Status       | soft-fork status of the block. The status is `active` when enough votes have been collected.

### Script Validation Rules Summary

Validation of scripts in blocks is defined for each release and depend on _validation
context_ which includes type of block, soft-fork status and script version. We denote
blocks being created by miners as `candidate` and those distributed across network as
`mined`.

Thus, we have 8 different validation contexts multiplied by 2 node versions
having in total 16 validation rules as summarized in the following table, which 
specifies the action a node have to take in the given contexts.

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
10   | active   | candidate | Script v1 | v5.0 | R5.0-JIT-verify 
11   | active   | candidate | Script v2 | v4.0 | skip-pool-tx (cannot handle)
12   | active   | candidate | Script v2 | v5.0 | R5.0-JIT-verify 
||||
13   | active   | mined     | Script v1 | v4.0 | skip-accept (rely on majority)
14   | active   | mined     | Script v1 | v5.0 | R5.0-JIT-verify 
15   | active   | mined     | Script v2 | v4.0 | skip-accept (rely on majority)
16   | active   | mined     | Script v2 | v5.0 | R5.0-JIT-verify 

Note the following properties of the validation rules.

1. Rules 1-4 specify creation of new candidate blocks _before_ soft-fork is activated. 
They require that the behaviour of v4.0 and v5.0 nodes should be identical.

2. Rules 9-10 specify creation of new candidate blocks _after_ soft-fork is activated.
They are different for v4.0 and v5.0 nodes, but
[equivalent](#equivalence-properties-of-validation-actions) with respect to preserving
consensus (see also [Rule Descriptions](#rule-descriptions) for details).

3. For any given tuple (`SF Status`, `Script Version`, `Release`) the _equivalent_ `ValidationAction` is
applied for both `candidate` and `mined` blocks. This proves the consistency of the rules
with respect to the change of the block status from `candidate` to `mined`, both before
and after soft-fork activation.

4. Each rule `i`, where `i` is an odd number, defines a `Validation Action` performed by
a v4.0 node. Each such rule is paired with the `i+1` rule which defines `Validation Action`
performed by a v5.0 node. Any such a pair `(i, i+1)` of rules have `Validation Actions`
which are either the same or equivalent with respect to the [Equivalence Properties of
Validation Actions](#equivalence-properties-of-validation-actions). This proves
consistency of validation actions across v4.0 and v5.0 nodes.

5. After SF is activated (`SF Status == active`), both AOT-cost and AOT-verify
implementations are no longer used in `Validation Action` of v5.0 nodes. The only context
where v5.0 node needs to use AOT based verification is given by Rule 6, which is to verify 
a v1 script in a historical mined block before SF is activated. 
However relying on _Prop 3_ we can replace Rule 6 in a new v5.0.1 release with the
following _equivalent_ rule

Rule#| SF Status | Block Type| Script Version | Release | Validation Action 
-----|-----------|-----------|----------------|---------|--------
17   | inactive  | mined     | Script v1      | v5.0.1  | R5.0-JIT-verify 

This will allow to remove AOT implementation in v5.0.1 and simplify reference
implementation significantly. 

### Rule Descriptions 

#### Rules 1 and 2
 _Handle v1 script in candidate block when SF is not active._
 Ensured by _Prop1_ and _Prop2_ both v4.0 and v5.0 nodes use equivalent AOT-cost and
 AOT-verify and thus have consensus.
 Release v5.0 will contain both AOT and JIT versions simultaneously and thus can _behave as v4.0_
 before SF is activated and _behave as v5.0_ after SF activation.
 
#### Rules 3 and 4
_Both v4.0 and v5.0 nodes reject v2 scripts in new candidate blocks when SF is NOT active._ 
This is ensured by _Prop5_ which is motivated by the following reasons: 
- v4.0 nodes have no implementation of v2 scripts, thus rejecting them altogether both in
  input and output boxes of new candidate blocks
- v5.0 nodes behave like v4.0 nodes and are waiting for majority of nodes to vote, thus
rejecting until SF is activated

#### Rules 5 and 6
 _Handle v1 script in mined block when SF is not active._
 Similar to rules 1 and 2 but for `mined` blocks. 
 
#### Rules 7 and 8
_Both v4.0 and v5.0 nodes reject v2 scripts in `mined` blocks when SF is NOT active._ 
Similar to rules 3 and 4.

#### Rules 9 and 10
These rules allow v1 scripts to enter blockchain even after SF is activated (for backward
compatibility with Apps).
Now, after SF is activated, the majority consist of v5.0 nodes and they will do
`R5.0-JIT-verify(Script v1)` which is equivalent to `R4.0-AOT-verify(Script v1)` due to 
_Prop3_.
 
To understand this pair of rules it is important to remember that a specific version of
ErgoTree (in this case v1) assumes the fixed semantics of all operations. This however
doesn't restrict the interpreter implementations and we use this fact
to switch from `R4.0-AOT-verify` to `R5.0-JIT-verify` relying on their equivalence
property _Prop 3_.

However, for backward compatibility with Apps we DON'T NEED equivalence of costing, hence 
exact cost estimation is not necessary. For this reason we have the relaxed condition in 
_Prop4_, which means that any ScriptV1 admitted by `R4.0-AOT-cost` will also be admitted by
`R5.0-JIT-cost`. For this reason, the v4.0 based application interacting with v5.0 node
will not notice the difference.

#### Rules 11 and 12
After SF is activated v4.0 node cannot verify transactions containing v2 scripts, as
a result the v4.0 node cannot include such transactions in new `candidate` blocks. Thus it
performs `skip-pool-tx` action essentially using only those mempool transactions which it
can handle. Majority of network nodes (v5.0) will do `R5.0-JIT-verify` validation of v2
scripts in `candidate` blocks.

#### Rules 13 and 14
After SF is activated v4.0 node `skip-accept` verification of `mined` blocks by relying on
the majority of v5.0, essentially not performing verification even of v1 scripts. The
majority of nodes uses new JIT based implementation of ErgoTree interpreter
`R5.0-JIT-verify` procedure for costing and verification, have consensus about blocks and
v4.0 nodes just accept all mined blocks created elsewhere.

#### Rules 15 and 16
After SF is activated v4.0 node `skip-accept` of `mined` blocks by relying on the majority
of v5.0 nodes since it cannot handle v2 scripts. In the same context, the majority of
nodes uses the new JIT based implementation of ErgoTree interpreter `R5.0-JIT-verify`
procedure for costing and verification.


### Equivalence Properties of Validation Actions

In order to guarantee network consensus in the presence of both v4.0 and v5.0 nodes
the implementation of `R4.0-AOT-verify`, `R5.0-AOT-verify`, `R4.0-AOT-cost`,
`R5.0-AOT-cost`, `R5.0-JIT-verify` should satisfy the following properties.

_Prop 1._ AOT-verify is preserved: `forall s:ScriptV1, R4.0-AOT-verify(s) == R5.0-AOT-verify(s)` 

_Prop 2._ AOT-cost is preserved: `forall s:ScriptV1, R4.0-AOT-cost(s) == R5.0-AOT-cost(s)`

_Prop 3._ JIT-verify can replace AOT-verify: `forall s:ScriptV1, R5.0-JIT-verify(s) == R4.0-AOT-verify(s)`

_Prop 4._ JIT-cost is bound by AOT-cost: `forall s:ScriptV1, R5.0-JIT-cost(s) <= R4.0-AOT-cost(s)`

_Prop 5._ ScriptV2 is rejected before SF is active:  
`forall s:ScriptV2, if not SF is active => R4.0-verify(s) == R5.0-verify(s) == Reject`

### Other Notes

- Since we are going to fix some bugs, the behaviour of v1 and v2 scripts in general not
required to be precisely equivalent. So while old apps are supported unchanged, new apps
are encouraged to use new ErgoScript frontend which will compile v2 scripts.

- Also, on v1 and v2, it would be better to avoid changing semantics of existing ops,
deprecation old and introducing new ones is cleaner