
## Estimation of ErgoTree computational complexity
 
### Background

To prevent DDoS attacks every script in a blockchain have to be checked for complexity limits.
This estimation happens during block/transaction validation for every guarding script of every input box.
Script can be executed iff its estimated complexity in a given `Context` is less than a `limit` value.

### Contract execution context

Transaction `tx` is validated as part of the block.
Every input box `ib` in `tx` contains a property `propBytes` with serialized ErgoTree of the contract.
During validation `propBytes` property is deserialized to ErgoTree `tree` which is executed.
The box `ib` itself is accessed via `SELF` property of the `Context` data structure.
Besides `Context` execution of a contract depends on votable `ProtocolParameters` data, which contains
global parameters which can be set up by miners following a voting protocol.

### Asymptotic complexity of the costing algorithm

For a given input box `ib` the algorithm consists of the following steps (details of each 
step are given in later sections):

`#` | Step                                                              | Complexity                 
----|-------------------------------------------------------------------|-----------
1   | Check that `val len = propBytes.length; len < MaxPropBytesSize`   | `O(1)` 
2   | Deserialize `propBytes` to ErgoTree `tree`  with `N` nodes        | `O(len) and N = O(len)` 
3   | Recursively traverse `tree` and build costed graph `graphC` with `M <= N` nodes | `O(N)`
4   | Split `graphC` into calculation function `calcF` and cost estimation function `costF` | `O(M)`
5   | Topoligically sort nodes of `costF` for execution (Tarjan algorithm) | `O(M)`
6   | Iterate over sorted nodes of `costF` and execute primitive ops    | `O(M)` 

### Deserialization (Steps 1, 2 of costing algorithm)

Deserializer should check that serialized byte array is of limited size, otherwise 
out-of-memory attack is possible. `MaxPropBytesSize` is a protocol parameter (see `ProtocolParameters`). 
During deserialization another parameter `MaxTreeDepth` is checked to limit depth of ErgoTree and thus 
avoid stack-overflow attack.

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
  def value: Val      // the value which is costed
  def cost: Int       // the cost estimation to obtain value
  def dataSize: Long  // the size estimation of the value in bytes
}
```
Every costed value `valC: Costed[T]` is a container for the value along with additional data to 
represent cost and size (costing information, costing properties).

Note, that `cost` and `dataSize` are independent parameters because some _costed_ values may have 
very small `dataSize`, but at the same time very high `cost`, e.g. result of contract may be `true` 
boolean value whose `dataSize` is 1 byte, but its `cost` is the cost of executing the whole contract. 
The opposite is also possible. For example a context variable of `Array[Byte]` type have `cost` equal 0,
but may have very big `dataSize`.

From this perspective Costed Graph `graphC` is a data flow graph between costed values.
Costed graph represents both execution of values and simultaneous execution of `cost` and `dataSize`
estimations for all intermediate values.

The `cost` and `dataSize` computations depend on operations in the contract. 
Consider the following contract fragment where we multiply two big integers:
```
val x: BigInt = ...
val y: BigInt = ...
val z = x * y
``` 
In the costed graph it corresponds to the following fragment:

```
val xC: Costed[BigInt] = ...
val yC: Costed[BigInt]= ...
val zC = new Costed { 
  val value = xC.value * yC.value
  val dataSize = xC.dataSize + yC.dataSize + 1L
  val cost = xC.cost + yC.cost + costOf("*_per_item") * this.dataSize
}
``` 
For the example above note the following properties: 
1) both `cost` and `dataSize` depend on costing information from arguments
2) resulting `zC.cost` depend on `dataSize` of arguments
3) neigher `cost` nor `dataSize` depend on argument values, i.e. costing properties of result
can be approximated using costing properties of arguments along. 

The property 3) turns out to be very important, because many operations have this property 
which leads to the possibility to do context-independent costing.

#### Type-dependent representation of costs

Depending on type `T` costed values have specialized representations, given by descendants of 
the type `Costed[T]`.
 
##### Primitive types

The simplest case is when `T` is primitive.
In this case cost information is represented by class `CCostedPrim[T]` derived from trait `CostedPrim[T]`. 
```
trait CostedPrim[Val] extends Costed[Val] 
class CCostedPrim[Val](val value: Val, val cost: Int, val dataSize: Long) extends CostedPrim[Val]
```
This representation of costs is used for intermediate values of type `Boolean`, `Byte`, `Short`, `Int`, 
`Long`, `BigInt`, `GroupElement` and `SigmaProp` types. 
These types represent atomic values of constant or limited size.
The following table summarizes this

Type           | Data Size, bytes
---------------|-----------------
`Boolean`      |  == 1 
`Byte`         |  == 1 
`Short`        |  == 2
`Int`          |  == 4
`Long`         |  == 8
`BigInt`       |  <= 32
`GroupElement` |  <= 32
`SigmaProp`    |  <= 32

Constants, context variables and registers of primitive types are represented in costed graph `graphC`
using `CCostedPrim` class. For these cases costed properties as computed from actual data values. 
For example having Int literal in the contract 

`val x: Int = 10`

costing algorithm constructs the following graph

`val xC: Costed[Int] = CCostedPrim(10, costOf("Const:() => Int"), sizeOf[Int])`

Here `costOf("Const:() => Int")` is an operation which requests `CostModel` for the cost of using 
a constant value in a script.
  
##### Tuple types

If `L` and `R` types, then costed value of type `(L,R)` is represented by the following
specializations of `Costed[(L,R)]` type
```
trait CostedPair[L,R] extends Costed[(L,R)] {
  def l: Costed[L]
  def r: Costed[R]
}
class CCostedPair[L,R](val l: Costed[L], val r: Costed[R]) extends CostedPair[L,R] {
  def value: (L,R) = (l.value, r.value)
  def cost: Int = l.cost + r.cost + ConstructTupleCost
  def dataSize: Long = l.dataSize + r.dataSize
}
```
Thus, for every intermediate value of type `(L,R)` we assume it has two components 
which are both costed values and accessible via properties `l` and `r` of 
`CostedPair` trait.

For each constant, context variable and register access the corresponding `CCostedPair` 
is computed from actual data values by recursively deconstructing them down to primitive 
types.
For example for the constant `(10, (10L, true))` the following costed value will be created 
in costed graph 
```
CCostedPair(
  CCostedPrim(10, costOf("Const:() => Int"), sizeOf[Int]),
  CCostedPair(
    CCostedPrim(10L, costOf("Const:() => Long"), sizeOf[Long]), 
    CCostedPrim(true, costOf("Const:() => Boolean"), sizeOf[Boolean])))
```

##### Array types

If `Item` is a type of array element, then costed value of type `Col[Item]` is 
represented by the following specializations of `Costed[Col[Item]]` type
```
trait CostedCol[Item] extends Costed[Col[Item]] {
  def values: Col[Item]  // items in the collection
  def costs: Col[Int]    // costs[i] is the cost to compute the values[i]
  def sizes: Col[Long]   // sizes[i] is a size of value[i]
  def valuesCost: Int    // accumulated cost to create a collection besides item costs
  def mapCosted[Res](f: Costed[Item] => Costed[Res]): CostedCol[Res] = rewritableMethod
  def foldCosted[B](zero: Costed[B], op: Costed[(B, Item)] => Costed[B]): Costed[B] = rewritableMethod
}
class CCostedCol[Item](
      val values: Col[Item], val costs: Col[Int],
      val sizes: Col[Long], val valuesCost: Int) extends CostedCol[Item] {
  def value: Col[Item] = values
  def cost: Int = valuesCost + costs.sum
  def dataSize: Long = sizes.sum
}
```
For constant, context variables and registers values of `Col` type the costing information 
of `CCostedCol` is computed from actual data.

Note methods `mapCosted` and `foldCosted`, these methods represent costed version of original
collection methods. Note the methods are defined as rewritable, meaning their implementation 
require special rewriting rule. See section [Rewrite Rules](#RewriteRules)

####
