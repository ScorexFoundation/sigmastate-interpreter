
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

### Costing Rules

The following constants are used in cost and size calculations.

Constant Name | Description
--------------|------------
GroupSize     | Number of bytes to represent any group element as byte array

The following table shows the rules for calculating cost and size for a result of each operation
based on costs and sizes of the operations arguments.
The operations names are given by the node classes of ErgoTree.


Operation            |  Cost in time units, Size in bytes
---------------------|-----------------------------------
`ProveDLog`          | CT("ProveDlogEval"), GroupSize 
`ProveDHTuple`       | CT("ProveDHTupleEval"), GroupSize * 4
x,y: BigInt; x op y where op in ("+", "-") | cost(x) + cost(y) + CT("op"), MaxSizeInBytes 


### Asymptotic complexity of the costing algorithm

For a given input box `ib` the algorithm consists of the following steps (details of each 
step are given in later sections):

`#` | Step                                                              | Complexity                 
----|-------------------------------------------------------------------|-----------
1   | Check that `val len = propBytes.length; len < MaxPropBytesSize`   | `O(1)` 
2   | Deserialize `propBytes` to ErgoTree `tree`  with `N` nodes        | `O(len) and N = O(len)` 
3   | Recursively traverse `tree` and build costed graph `graphC` with `M <= N` nodes | `O(N)`
4   | Split `graphC` into calculation function `calcF` and cost estimation function `costF` | `O(M)`
5   | Topologically sort nodes of `costF` for execution (Tarjan algorithm) | `O(M)`
6   | Iterate over sorted nodes of `costF` and execute primitive ops    | `O(M)` 

#### Overview of Costing Process 
<a name="CostingOverview"></a> 

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
1) Building of _Costed Graph_ `graphC` (see [below](#BuildingCostedGraph))
2) Splitting of the `graphC` into `calcF` and `costF` functions (see [below](#SplittingCostedGraph))

### Deserialization (Steps 1, 2 of costing algorithm)

Deserializer should check that serialized byte array is of limited size, otherwise 
out-of-memory attack is possible. `MaxPropBytesSize` is a protocol parameter (see `ProtocolParameters`). 
During deserialization another parameter `MaxTreeDepth` is checked to limit depth of ErgoTree and thus 
avoid stack-overflow attack.

### Building Costed Graph (Step 3)
<a name="BuildingCostedGraph"></a> 

Costed Graph is a graph-based intermediate representatin (IR) which is created from the deserialized 
ErgoTree. Implementation of Costed Graph is based on Scalan/Special framework.
See [Scalan idioms](https://github.com/scalan/scalan.github.io/blob/master/idioms.md) for details.

#### Costed Values
<a name="CostedValues"></a> 

The nodes of the costed graph `graphC` are _costed values_ of type `Costed[T]`.
```scala
trait Costed[Val] {
  def value: Val      // the value which is costed
  def cost: Int       // the cost estimation to obtain value
  def dataSize: Long  // the size estimation of the value in bytes
}
```
Every costed value `valC: Costed[T]` is a container of the value along with additional data to 
represent cost and size (costing information, costing properties).

Note, that `cost` and `dataSize` are independent parameters because some _costed_ values may have 
very small `dataSize`, but at the same time very high `cost`, e.g. result of contract may be `true` 
boolean value whose `dataSize` is 1 byte, but its `cost` is the cost of executing the whole contract. 
The opposite is also possible. For example a context variable of `Coll[Byte]` type have `cost` equal 0,
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

```scala
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

Depending on type `T` costed values have specialized representations, given by descendants of 
the type `Costed[T]`. The following subsections describe possible specialization in detail.
 
##### Costed Values of Primitive Type

The simplest case is when `T` is primitive.
In this case cost information is represented by class `CCostedPrim[T]` derived from trait `CostedPrim[T]`.
Separation into class and closely related trait is technical implementation detail, we will omit traits 
in the following sections for brevity. The trait always have the same public methods as class. 
```scala
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
  
##### Costed Values of Tuple type

If `L` and `R` types, then costed value of type `(L,R)` is represented by the following
specializations of `Costed[(L,R)]` type
```scala
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
```scala
CCostedPair(
  CCostedPrim(10, costOf("Const:() => Int"), sizeOf[Int]),
  CCostedPair(
    CCostedPrim(10L, costOf("Const:() => Long"), sizeOf[Long]), 
    CCostedPrim(true, costOf("Const:() => Boolean"), sizeOf[Boolean])))
```

##### Costed Values of Coll Type

If `Item` is a type of array element, then costed value of type `Coll[Item]` is 
represented by the following specializations of `Costed[Coll[Item]]` type
```scala 
class CCostedColl[Item](
      val values: Coll[Item], val costs: Coll[Int],
      val sizes: Coll[Long], val valuesCost: Int) extends CostedColl[Item] {
  def value: Coll[Item] = values
  def cost: Int = valuesCost + costs.sum
  def dataSize: Long = sizes.sum
  def mapCosted[Res](f: Costed[Item] => Costed[Res]): CostedColl[Res] = rewritableMethod
  def foldCosted[B](zero: Costed[B], op: Costed[(B, Item)] => Costed[B]): Costed[B] = rewritableMethod
}
```
For constant, context variables and registers values of `Coll` type the costing information 
of `CCostedColl` is computed from actual data.

Note methods `mapCosted` and `foldCosted`, these methods represent costed version of original
collection methods. Note the methods are defined as rewritable, meaning their implementation 
require special rewriting rule. See section [Rewrite Rules](#RewriteRules)

#### Building Costed Graph 

Given an environment `envVals` and ErgoTree `tree` a Costed Graph can be obtained as 
[reified lambda](https://github.com/scalan/scalan.github.io/blob/master/idioms.md#Idiom4) of type 
`Rep[Context => Costed[T#WrappedType]]`
This transformation is implemented as shown in the following `buildCostedGraph` method, which can
be found in `RuntimeCosting.scala` file.
```scala
def buildCostedGraph[T <: SType](envVals: Map[Any, SValue], tree: Value[T]): Rep[Context => Costed[T#WrappedType]] = 
  fun { ctx: Rep[Context] =>        // here ctx represents data context
    val ctxC = RCCostedContext(ctx) // data context is wrapped into Costed value container 
    val env = envVals.mapValues(v => evalNode(ctxC, Map(), v)) // do costing of environment
    val res = evalNode(ctxC, env, tree)  // traverse tree recursively applying costing rules 
    res
  }
```
Note that function `evalNode` applies the [costing rules](#CostingRules) recursively for tree nodes 
(of `sigmastate.Values.Value[T]` type). Those rules are executed in the `fun` block and 
as result all the created graph nodes belong to the resulting costed graph

#### Costing Rules
<a name="CostingRules"></a> 

In order to build costed graph, the algorithm have to recursively traverse ErgoTree. 
For each node of ErgoTree, separate _costing rule_ is applied using `evalNode` method 
whose structure is show below. 
```scala
type RCosted[A] = Rep[Costed[A]]
type CostingEnv = Map[Any, RCosted[_]]
def evalNode[T <: SType](ctx: Rep[CostedContext], env: CostingEnv, node: Value[T]): RCosted[T#WrappedType] = {
  def eval[T <: SType](node: Value[T]) = evalNode(ctx, env, node)
  object In { def unapply(v: SValue): Nullable[RCosted[Any]] = Nullable(evalNode(ctx, env, v)) }
  ...
  node match {
    case Node1(In(arg1),...,In(argK)) => rhs1(arg1,...,argK)
    ...
    case NodeN(In(arg1),...,In(argK)) => rhsN(arg1,...,argK)
  }
}
```
Here `In` is a helper extractor which recursively apply `evalNode` for each argument so that variables 
`arg1,...,argK` represent results of costing of the corresponding subtree. 
The right hand side of each rule (`rhs1,...rhsN`) contains operations with 
[Rep types](https://github.com/scalan/scalan.github.io/blob/master/idioms.md#Idiom3), the effect of their 
execution is creation of new graph nodes which become part of resulting costed graph.

Following is an example of a simple costing rule to introduce basic concepts 
(it can be found in RuntimeCosting.scala file).
```scala
  case sigmastate.MultiplyGroup(In(_l), In(_r)) =>
    val l = asRep[Costed[WECPoint]](_l)   // type cast to an expected Rep type
    val r = asRep[Costed[WECPoint]](_r)
    val value = l.value.add(r.value)            // value sub-rule
    val cost = l.cost + r.cost + costOf(node)   // cost sub-rule
    val size = CryptoConstants.groupSize.toLong // size sub-rule
    RCCostedPrim(value, cost, size)
```
The rule first recognizes specific ErgoTree node, then recursively each argument is costed 
so that the result is bound with variables `_l` and `_r`. 
Right hand side starts with typecasting costed subgraphs to expected types. This operation is safe 
because input ErgoTree is type checked. These typecasts also make the rest of the rules typesafe,
which is ensured by Scala compiler checking correctness of the operations.
Using costed arguments `l` and `r` the rule contains three sub-rules.
_Value rule_ implements calculation of the resulting value and essentially translates  
MultiplyGroup operation into operation in the costed graph.
_Cost rule_ defines formula of the cost for `MultiplyGroup` operation (by adding to the costed graph).
_Size rule_ defines formula of the size for `MultiplyGroup` operation
And finally `value`, `cost` and `size` are packed into costed value which represent current tree node 
in the costed graph.

The rule above has the simples form and applicable to most simple operations. However some operations
require rules which don't fall into this default patterns. 
Following is an example rule for `MapCollection` tree node, which makes recursive costing of arguments
explicit by using `eval` helper and also employ other idioms of staged evaluation.
```scala
  case MapCollection(input, id, mapper) =>
    val eIn = stypeToElem(input.tpe.elemType)   // translate sigma type to Special type descriptor
    val xs = asRep[CostedColl[Any]](eval(input)) // recursively build subgraph for input argument
    implicit val eAny = xs.elem.asInstanceOf[CostedElem[Coll[Any],_]].eVal.eA
    assert(eIn == eAny, s"Types should be equal: but $eIn != $eAny")
    val mapperC = fun { x: Rep[Costed[Any]] => // x argument is already costed
      evalNode(ctx, env + (id -> x), mapper)   // associate id in the tree with x node of the graph
    }
    val res = xs.mapCosted(mapperC)    // call costed method of costed collection
    res
```
Observe that this rule basically translates mapping operation over collection into invocation of
the method `mapCosted` on costed collection value `xs` with appropriately prepared argument `mapperC`.
Because `xs` has [Rep type](https://github.com/scalan/scalan.github.io/blob/master/idioms.md#Idiom3)
this invocation has an effect of adding new `MethodCall(xs, "mapCosted", Seq(mapperC))` node to the graph.
This new node is immediately catched by the rewriting rule (see [Rewrite Rules](#RewriteRules) section) which further transforms it into final 
nodes of resulting costed graph. Such separation makes the whole algorithm more modular. 

### Spliting Costed Graph (Step 4)
<a name="SplittingCostedGraph"></a> 

Costed Graph represents simultaneous calculation of original contract and cost information along contract's data flow.
However in order to perform cost estimation before contract calculation we need to separate contract calculation 
operations of the Costed Graph from cost and data size computing operations. 

After building a Costed Graph
```scala
val graphC: Rep[Context => SType#WrappedValue] = buildCostedGraph(env, tree)
```
we can perform _splitting_ by using the function `split` to obtain `calcF` and `costF` functions
```scala
val Pair(calcF: Rep[Context => SType#WrappedValue], costF: Rep[Context => Int]) = split(graphC)
```
Both `calcF` and `costF` take `Context` as its argument and both represented as 
[reified lambdas](https://github.com/scalan/scalan.github.io/blob/master/idioms.md#Idiom4) of
Scalan/Special IR.

This _splitting_ function is generic and defined as shown below 
```scala
  def split[T,R](f: Rep[T => Costed[R]]): Rep[(T => R, T => Int)] = {
    val calc = fun { x: Rep[T] => val y = f(x); val res = y.value; res }
    val cost = fun { x: Rep[T] => f(x).cost }
    Pair(calc, cost)
  }
```
In order to understand how this function works first observe, that this function is from 
reified lambda to a pair of reified lambdas, thus it performs transformation of one graph to a pair of 
new graphs.
Second, consider the `fun` block in the definition of `calc` and the application `f(x)` inside the block.
Because `f` is a reified lambda then according to staged evaluation semantics its application to `x`
has an effect of inlining, i.e. unfolding the graph of `f` with `x` substituted for `f`'s argument into 
the block of `fun`.
Third, remember that Costed Graph have nodes of types derived from `Costed` depending of the type of value,
e.g. for primitive type `CCostedPrim(v, c, s)` node is added to the graph.
This is described in [Costed Values](#CostedValues) section.
And forth, recall that typical costing rule have the following formulas for calculation of costed values.
```scala
    val value = l.value.add(r.value)            // value sub-rule
    val cost = l.cost + r.cost + costOf(node)   // cost sub-rule
    val size = CryptoConstants.groupSize.toLong // size sub-rule
    RCCostedPrim(value, cost, size)
```
Combined with third point and assuming 
```scala
  l = new CCostedPrim(v1, c1, s1)
  r = new CCostedPrim(v2, c2, s2)
``` 
we can conclude that `l.value.add(r.value)` will evaluate to `v1.add(v2)` and similarly
`l.cost + r.cost + costOf(node)` will evaluate to `c1 + c2 + costOf(node)`.

```scala
    val value = v1.add(v2)            // value sub-rule
    val cost = c1 + c2 + costOf(node)   // cost sub-rule
    val size = CryptoConstants.groupSize.toLong // size sub-rule
    RCCostedPrim(value, cost, size)
``` 
In other words, resuting node of the cosing rule doesn't depend on intermediate costed nodes.
Such intermediate nodes (like `CCostedPrim` above) become dead and are not used in values calculation.
This is the key insight in the implementation of function `split`.

Now, keeping above in mind, after the graph of `f` is unfolded `y` represents resulting node. 
Thus, `value` property is called on the costed node `y` which has type `Costed`. 
The resulting symbol obtained by execution of `y.value` becomes a resulting node of the
`calc` graph. After the block of `fun` operator is executed, dead-code-elimination is performed by `fun`
using simple collection of reachable nodes of the graph starting from resulting node `res`.

Thus, by construction, the body of `calc` contains only operations necessary to execute contract, and have no
vestiges of costing and sizing operations.

### Real World Complications

Here we discuss some complications in the algorithm caused by the diversity of real world examples.
The main motivation here is to keep main algorithm generic and simple, and encapsulate all complexity 
of the specific cases into reusable modules.

<a name="RewriteRules"></a> 
#### Rewrite Rules

[Rewrite rules](https://github.com/scalan/scalan.github.io/blob/master/idioms.md#Idiom6) is the mechanism 
to hook into graph building process and perform on the fly substitution of
specific sub-graphs with equivalent but different sub-graphs.
The following rule uses auto-generated extractor `mapCosted` which recognizes invocations of method 
`CostedColl.mapCosted` (Remember, this method was used in costing rule for `MapCollection` tree node).

```scala
override def rewriteDef[T](d: Def[T]): Rep[_] = {
  val CCM = CostedCollMethods
  d match {
  case CCM.mapCosted(xs: RCostedColl[a], _f: RCostedFunc[_, b]) =>
    val f = asRep[Costed[a] => Costed[b]](_f)
    val (calcF, costF, sizeF) = splitCostedFunc[a, b](f)
    val vals = xs.values.map(calcF)
    val mRes = AllMarking(element[Int])
    val mCostF = sliceAnalyzer.analyzeFunc(costF, mRes)
    implicit val eA = xs.elem.eItem
    implicit val eB = f.elem.eRange.eVal

    val costs = mCostF.mDom match {
      case PairMarking(markA,_) if markA.isEmpty =>
        val slicedCostF = fun { in: Rep[(Int, Long)] => costF(Pair(variable[a], in)) }
        xs.costs.zip(xs.sizes).map(slicedCostF)
      case _ =>
        xs.values.zip(xs.costs.zip(xs.sizes)).map(costF)
    }
    val tpeB = elemToSType(eB)
    val sizes = if (tpeB.isConstantSize) {
      colBuilder.replicate(xs.sizes.length, typeSize(tpeB))
    } else
      xs.sizes.map(sizeF)
    RCCostedColl(vals, costs, sizes, xs.valuesCost)
  case _ => super.rewriteDef(d)
  }
}
```
Method `rewriteDef` is defined in `Scalan` cake trait and can be overridden following _stackable overrides_  
pattern (calling super.rewriteDef for the default case). This specific rule is defined in `RuntimeCosting`
trait.
