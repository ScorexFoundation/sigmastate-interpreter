# ErgoScript Language Description

## Introduction

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
- val declarations: `val h = blake2b256(pubkey)`
- if-then-else clause: `if (x > 0) 1 else 0`
- array literals: `Array(1, 2, 3, 4)`
- generic high-order array operations: `map`, `fold`, `exists`, `forall`, etc.
- accessing fields of any predefined structured objects: `box.value`
- function invocations (predefined and user defined): `proveDlog(pubkey)` 
- user defined functions: `def isValid(pk: GroupElement) = proveDlog(pk)`
- lambdas and high-order methods: `OUTPUTS.exists { (out: Box) => out.value >= minToRaise }`

#### Data types 

- `Any` - a supertype of any other type
- `Unit` - a type with a single value `()`
- `Int` - 64 bit signed integer
- `Boolean` - a type with two logical values `true` and `false`
- `SigmaProp`  - a type which represent a logical value which can be be obtained by 
             executing a Sigma protocol with zero-knowledge proof of knowledge
- `BigInt`  - immutable arbitrary-precision integers
- `ByteArray` - arbitrary sequence of bytes
- `AvlTree`
- `GroupElement` - elliptic curve points
- `Box` - a box containing a value with guarding proposition
- `Option[T]` - a container which either have some value of type `T` or none.
- `Array[T]` - arrays of arbitrary length with all values of type `T` 
- `(T1, ..., Tn)` - tuples

#### Literal syntax and Constants

There is a syntax for literals, which can be used to introduce values 
of some types directly in program text like the following examples:
```
 val unit: Unit = ()       // unit constant
 val long: Int = 10       // interger value literal
 val bool: Boolean = true  // logical literal
 val arr = Array(1, 2, 3)  // constructs array with given items
```
Note that many types don't have literal syntax and their values are introduced 
by applying operations.

#### Context Data 

The following data objects available in every script using predefined variables 

- `HEIGHT: Int` - height (block number) of the current block
- `SELF: Box` - block to be opened when currently executing script evaluates to `true`
- `INPUTS: Array[Box]` - an array of inputs of the current spending transaction
- `OUTPUTS: Array[Box]` - an array of outputs of current spending transaction
- `LastBlockUtxoRootHash: AvlTree` - last block UTXO root 

## Data Types

### Predefined data structures

#### Box

Every box has the following properties: 

- `id: ByteArray` - Blake2b256 hash of this box's content 
- `value: Int` - boxed value
- `propositionBytes: ByteArray` - guarding script, which should be evaluated to true in order to open this box 

Besides properties, every box can have up to 10 numbered registers.
The following syntax is supported to access registers on box objects:
```
SELF.R3[Int].get            // access R3 register, cast its value to Int and return it
SELF.R3[Int].isDefined      // check that value of R3  is defined and has type Int
SELF.R3[Int].isEmpty        // check that value of R3  is undefined 
SELF.R3[Int].getOrElse(def) // access R3 value if defined, otherwise return def
```
Note, that Option[T] is introduced at frontend to represent the type of register value
```
SELF.R3: Option[Any]   // where Any is the supertype of all types
SELF.R3[Int]: Option[Int]   
```
However, Option is not supported by `Interpreter`, so all `Option` operations are eliminated during compilation.

#### Array[T]

As in many languages Array is a collection of items of the same type. 
`Array[T]` - is a collection of items of type `T`.

Each item can be accessed by constant index, for example:
```
val myOutput = OUTPUTS(0)
val myInput = INPUTS(0)
```

Array have `size` property which returns number of elements in an array. 

```
val size = OUTPUTS.size
```

Even though ErgoScript is not object-oriented language, Array operations use the syntax of method invocations. 
For example the following script check an existence of some element in the array satisfying some predicate (condition)

```
val ok = OUTPUTS.exists { (box: Box) => box.value > 1000 }
``` 

The following properties and methods can be used with arrays

Function  | Description
--------- | ------------
`Array[T].size` |  number of items in the array
`def Array[T].exists(p: T => Boolean): Boolean ` | Returns true if there exists an item `x` in the array for which `p(x) == true`  
`def Array[T].forall(f: T => Boolean): Boolean ` | Returns true if for all items `x` in the array `p(x) == true`   
`def Array[T].map[R](f: T => R): Array[R] ` | Applies function `f` for each element of array collecting results in a new array of type `R`. 
`def Array[T].reduce(f: (T, T) => T): T ` | For an array `Array(a0, ..., aN)` computes `f(f( ...f(f(a0, a1), a2) ...), aN)`. 

#### AvlTree
TBD

### Predefined global functions

ErgoScript standard library include predefined functions that can be called 
without prior declaration. 

The following function declarations are automatically imported into any script:

```
/** Returns true if all the conditions are true */
def allOf(conditions: Array[Boolean]): Boolean

/** Returns true if any of the conditions is true */
def anyOf(conditions: Array[Boolean]): Boolean

/** Cryptographic hash function Blake2b */
def blake2b256(input: Array[Byte]): Array[Byte]

/** Cryptographic hash function Sha256 */
def sha256(input: Array[Byte]): Array[Byte]

/** Create BigInt from byte array representation. */
def byteArrayToBigInt(input: Array[Byte]): BigInt

/** Returns bytes representation of integer value. */
def intToByteArray(input: Int): Array[Byte]

/** Returns value of the given type from the environment by its tag.*/
def getVar[T](tag: Int): Option[T]

def proveDHTuple(g: GroupElement, h: GroupElement, 
                 u: GroupElement, v: GroupElement): Boolean
def proveDlog(value: GroupElement): Boolean

/** Predicate which checks whether a key is in a tree, by using a membership proof. */
def isMember(tree: AvlTree, key: Array[Byte], proof: Array[Byte]): Boolean

/** Deserializes values from Base58 encoded binary data at compile time */
def deserialize[T](string: String): T
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
                   backerPubKey: SigmaProp, projectPubKey: SigmaProp) {
  val c1 = HEIGHT >= timeout && backerPubKey
  val c2 = allOf(Array(
    HEIGHT < timeout,
    projectPubKey,
    OUTPUTS.exists { (out: Box) => 
      out.value >= minToRaise && out.propositionBytes == projectPubKey.propBytes
    }
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
guard DemurrageCurrency(demurragePeriod: Int, demurrageCost: Int, regularScript: SigmaProp) {
  val c2 = allOf(Array(
   HEIGHT >= SELF.R3[Int].get + demurragePeriod,
   OUTPUTS.exists { (out: Box) => 
     out.value >= SELF.value - demurrageCost && out.propositionBytes == SELF.propositionBytes
   }
 ))
 regularScript || c2
}
```

#### Ring Signature

Simplest linear-sized ring signature (1-out-of-N OR). 
The ring is an array of public keys which correspond to some secret keys (of some parties).
The script below checks that at least one party provided the signature WITHOUT disclosing this party.

```
guard RingSignature(ring: Array[SigmaProp]) {
  anyOf(ring)
}
```

