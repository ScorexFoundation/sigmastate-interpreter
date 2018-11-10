
## Estimation of ErgoTree computational complexity
 
### Background

To prevent DDoS attacks every script in a blockchain have to be checked for complexity limits.
This estimation happens during block/transaction validation for every guarding script of every input box.
Script can be executed iff its estimated complexity in a given `Context` is less than a `limit` value.

### Script execution context

Transaction `tx` is validated as part of the block.
Every input box `ib` in `tx` contains a property `propBytes` with serialized ErgoTree.
During validation `propBytes` property is deserialized to ErgoTree `tree` which is executed.
The box `ib` itself is accessed via `SELF` property of the `Context` data structure.

### Asymptotic complexity of the algorithm

For a given input box `ib` the algorithm consists of the following steps:

`#` | Step                                                              | Complexity                 
----|-------------------------------------------------------------------|-----------
1   | Check that `val len = propBytes.length; len < MaxPropBytesSize`   | `O(1)` 
2   | Deserialize `propBytes` to ErgoTree `tree`  with `N` nodes        | `O(len) and N = O(len)` 
3   | Recursively traverse `tree` and build costed graph `graphC` with `M <= N` nodes | `O(N)`
4   | Split `graphC` into calculation function `calcF` and cost estimation function `costF` | `O(M)`
5   | Topoligically sort nodes of `costF` for execution (Tarjan algorithm) | `O(M)`
6   | Iterate over sorted nodes of `costF` and execute primitive ops    | `O(M)` 

### Graph-based IR

After deserialization ErgoTree is transformed to graph-based IR of Scalan framework.
See (Scalan idioms)[https://github.com/scalan/scalan.github.io/blob/master/idioms.md] for details.

### Costing Process 

Deserialized ErgoTree have to be translated into two related functions: 
1) `calcF: Context => SigmaBoolean` - script calculation function, which produces Sigma tree for 
further proof generation (when new `tx` is created) or proof verification (when `tx` is verified)
2) `costF: Context => Int` - cost estimation function, which by construction is closely connected 
with `calcF` and allows to compute execution complexity of `calcF` in a given context.

_Costing Process_ or simply _costing_ is the process of obtaining two functions `calcF` and `costF` 
for a given deserialized ErgoTree.

The key feature of the costing algorithm is that in many cases the functions `calcF` and `costF` can be 
constructed for a given ErgoTree once and for all Context. This is statically verifiable property
which can be ensured by the compiler of ErgoTree.

If context independent costing is not possible, the corresponding bit should be setup in script header.
In this case _context-dependent costing_ should be performed for the script during transaction validation.
Context-dependent costing can use data in the `Context` to construct `calcF` and `costF` functions.
This is necessary to achieve better cost approximations in complex scripts. 

Costing process is divided into two steps:
1) Construction of _Costed Graph_ `graphC`
2) Splitting of the `graphC` into `calcF` and `costF` functions

#### Costed Values

The nodes of the costed graph `graphC` are _costed values_ of type `Costed[T]`.
```
trait Costed[Val] {
  def value: Val
  def cost: Int
  def dataSize: Long
}
```

