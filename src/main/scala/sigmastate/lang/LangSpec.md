#ErgoScript Language Description

##Introduction

#### ErgoScript language features

- syntax borrowed from Scala and TypeScript
- standard semantics for well known constructs
- high-order with first-class lambdas
- call-by-value (eager evaluation)
- statically typed with local type inference
- blocks are expressions 
- semicolon inference in blocks
- type constructors: Tuple, Array, Option

#### Operations and constructs

- Binary operations: `>, <, >=, <=, +, -, &&, ||, ==, !=, |, *, ^, ++`
- predefined primitives: `blake2b256`, `byteArrayToBigInt`, `proveDlog` etc. 
- let declarations: `let h = blake2b256(pubkey)`
- if-then-else clause: `if (x > 0) 1 else 0`
- array literals: `Array(1, 2, 3, 4)`
- generic high-order array operations: `map`, `fold`, `exists`, `forall`, etc.
- accessing fields of any predefined structured objects: `box.value`
- function invocations (predefined and user defined): `proveDlog(pubkey)` 
- user defined functions: `let isValid(pk: GroupElement) = proveDlog(pk)`
- lambdas and high-order methods: `OUTPUTS.exists(fun (out: Box) = out.value >= minToRaise)`

#### Data types 

- `Any` - a supertype of any other type
- `Unit` - a type with a single value `()`
- `Long` - 64 bit signed integer
- `Boolean` - a type with two logical values `true` and `false`
- `BigInt`  - immutable arbitrary-precision integers
- `ByteArray` - arbitrary sequence of bytes
- `AvlTree`
- `GroupElement` - elliptic curve points
- `Box` - a box containing a value with guarding proposition
- `Option[T]` - a container which either have some value of type `T` or none.
- `Array[T]` - arrays of arbitrary length with all values of type `T` 
- `(T1, ..., Tn)` - tuples

####Literal syntax and Constants

There is a syntax for literals, which can be used to introduce values 
of some types directly in program text like the following examples:
```
 let unit: Unit = ()       // unit constant
 let long: Long = 10       // interger value literal
 let bool: Boolean = true  // logical literal
 let arr = Array(1, 2, 3)  // constructs array with given items
```
Note that many types don't have literal syntax and their values are introduced 
by applying operations.

#### Context Data 

The following data objects available in every script using predefined variables 

- `HEIGHT: Array[Box]` - height (block number) of the current block
- `SELF: Box` - block to be opened when currently executing script evaluates to `true`
- `INPUTS: Array[Box]` - an array of inputs of the current spending transaction
- `OUTPUTS: Array[Box]` - an array of outputs of current spending transaction
- `LastBlockUtxoRootHash: AvlTree` - last block UTXO root 

## Data Types

#### Predefined data structures

##### Box
TBD

##### AvlTree
TBD

#### Predefined global functions

ErgoScript standard library include predefined functions that can be called 
without prior declaration. 

The following function declarations are automatically imported into any script:

```
/** Returns true if all the conditions are true */
fun allOf(conditions: Array[Boolean]): Boolean

/** Returns true if any of the conditions is true */
fun anyOf(conditions: Array[Boolean]): Boolean

/** Cryptographic hash function Blake2b */
fun blake2b256(input: ByteArray): ByteArray

/** Create BigInt from byte array representation. */
fun byteArrayToBigInt(input: ByteArray): BigInt

/** Returns bytes representation of integer value. */
fun intToByteArray(input: Int): ByteArray

/** Returns valued from environment by its tag*/
fun taggedByteArray(tag: Int): ByteArray
fun taggedInt(tag: Int): Int
fun taggedBigInt(tag: Int): BigInt
fun taggedBox(tag: Int): Box
fun taggedGroupElement(tag: Int): GroupElement
fun taggedAvlTree(tag: Int): AvlTree
fun taggedBoolean(tag: Int): Boolean

fun proveDHTuple(g: GroupElement, h: GroupElement, 
                 u: GroupElement, v: GroupElement): Boolean
fun proveDlog(value: GroupElement): Boolean

/** Predicate which checks whether a key is in a tree, by using a membership proof. */
fun isMember(tree: AvlTree, key: ByteArray, proof: ByteArray): Boolean
```

## Examples

#### Crowd Funding

A project declares a need to raise "minToRaise" amount of tokens until some "timeout" 
height. A backer then creates an output which is spendable by with project's public key
until timeout and only if a spending transaction creates an output to project's 
public key with amount >= minToRaise. 
After the timeout output could be spent by backer only.
    
```
guard CrowdFunding(timeout: Int, minToRaise: Int, 
                   backerPubKey: Sigma, projectPubKey: Sigma) {
  let c1 = HEIGHT >= timeout && backerPubKey
  let c2 = allOf(Array(
    HEIGHT < timeout,
    projectPubKey,
    OUTPUTS.exists(fun (out: Box) = {
      out.value >= minToRaise && out.propositionBytes == projectPubKey.propBytes
    })
  ))
  c1 || c2
}
```

#### Demurrage Currency

The idea is that miners enforce users to combine a guarding script of a user 
(`regularScript` parameter) with a condition that anyone (presumably, a miner) 
can spend no more `demurrageCost` amount of tokens from an output of the user
after `demurragePeriod` blocks since output creation. 

If the user is relocating the money from the output before that height, 
the miner can charge according to output lifetime.

We assume that it is enforced by a consensus protocol to store height when 
an input got into a block in the register R3 (if the transaction is not included 
into the blockchain yet, then R3 contains the current height of the blockchain).
    
```
guard CrowdFunding(demurragePeriod: Int, demurrageCost: Int, regularScript: Sigma) {
  let c2 = allOf(Array(
   HEIGHT >= SELF.R3[Int].value + demurragePeriod,
   OUTPUTS.exists(fun (out: Box) = {
     out.value >= SELF.value - demurrageCost && out.propositionBytes == SELF.propositionBytes
   })
 ))
 regularScript || c2
}
```

#### Ring Signature

Simplest linear-sized ring signature (1-out-of-N OR), all secrets are known.

```
guard CrowdFunding(ring: Array[Sigma]) {
  anyOf(ring)
}
```

