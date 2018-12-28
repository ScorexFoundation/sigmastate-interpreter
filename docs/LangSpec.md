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
- type constructors: Tuple, Col, Option

#### Operations and constructs

- Binary operations: `>, <, >=, <=, +, -, &&, ||, ==, !=, |, *, ^, ++`
- predefined primitives: `blake2b256`, `byteArrayToBigInt`, `proveDlog` etc. 
- val declarations: `val h = blake2b256(pubkey)`
- if-then-else clause: `if (x > 0) 1 else 0`
- collection literals: `Col(1, 2, 3, 4)`
- generic high-order collection operations: `map`, `fold`, `exists`, `forall`, etc.
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
- `Col[T]` - a collection of arbitrary length with all values of type `T` 
- `(T1, ..., Tn)` - tuples

#### Literal syntax and Constants

There is a syntax for literals, which can be used to introduce values 
of some types directly in program text like the following examples:
```
 val unit: Unit = ()       // unit constant
 val long: Int = 10        // interger value literal
 val bool: Boolean = true  // logical literal
 val arr = Coll(1, 2, 3)    // constructs a collection with given items
```
Note that many types don't have literal syntax and their values are introduced 
by applying operations.

#### Context Data 

The following data objects available in every script using predefined variables 

- `HEIGHT: Int` - height (block number) of the current block
- `SELF: Box` - block to be opened when currently executing script evaluates to `true`
- `INPUTS: Col[Box]` - a collection of inputs of the current spending transaction
- `OUTPUTS: Col[Box]` - a collection of outputs of current spending transaction
- `LastBlockUtxoRootHash: AvlTree` - last block UTXO root 

## Data Types

### Predefined data structures

#### Box

Every box has the following properties: 

- `id: ByteArray` - Blake2b256 hash of this box's content 
- `value: Int` - boxed value
- `propositionBytes: ByteArray` - guarding script, which should be evaluated to true in order to open this box 
- `creationInfo: (Long, ByteArray)` - height when block got included into the blockchain and also transaction identifier and box index in the transaction outputs

```scala
/** Extracts register as Coll[Byte], deserializes it to script and then executes this script in the current context. 
  * The original Coll[Byte] of the script is available as getReg[Coll[Byte]](id)
  * @param id identifier of the register 
  * @tparam T result type of the deserialized script. 
  * @throws InterpreterException if the actual script type doesn't conform to T
  * @return result of the script execution in the current context
  * @since 2.0
  */
def deserializeFromRegister[T](id: Byte): T
```

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

#### Col[T]

As in many languages Array is a collection of items of the same type. 
`Col[T]` - is a collection of items of type `T`.

Each item can be accessed by constant index, for example:
```
val myOutput = OUTPUTS(0)
val myInput = INPUTS(0)
```

Any collection have a `size` property which returns number of elements in a collection. 

```
val size = OUTPUTS.size
```

Even though ErgoScript is not an object-oriented language, Col operations use the syntax of method invocations. 
For example the following script check an existence of some element in the collection satisfying some predicate (condition)

```
val ok = OUTPUTS.exists { (box: Box) => box.value > 1000 }
``` 

The following properties and methods can be used with arrays

Function  | Description
--------- | ------------
`Col[T].size` |  a number of items in the collection
`def Col[T].exists(p: T => Boolean): Boolean ` | Returns true if there exists an item `x` in the collection for which `p(x) == true`  
`def Col[T].forall(f: T => Boolean): Boolean ` | Returns true if for all items `x` in the collection `p(x) == true`   
`def Col[T].map[R](f: T => R): Col[R] ` | Applies function `f` for each element of the collection gathering results in a new collection of type `R`. 
`def Col[T].reduce(f: (T, T) => T): T ` | For a collection `Col(a0, ..., aN)` computes `f(f( ...f(f(a0, a1), a2) ...), aN)`. 

#### AvlTree
TBD

### Predefined global functions

ErgoScript standard library include predefined functions that can be called 
without prior declaration. 

The following function declarations are automatically imported into any script:

```scala 
/** Returns true if all the elements in collection are true. */
def allOf(conditions: Coll[Boolean]): Boolean

/** Returns true if at least on element of the conditions is true */
def anyOf(conditions: Coll[Boolean]): Boolean

/** Similar to allOf, but performing logical XOR operation instead of `||` 
  * @since 2.0
  */
def xorOf(conditions: Coll[Boolean]): Boolean 

/** Returns SigmaProp value which can be ZK proven to be true 
 * if at least k properties can be proven to be true. 
 */
def atLeast(k: Int, properties: Coll[SigmaProp]): SigmaProp
    
/** Special function to represent explicit Zero Knowledge Scope in ErgoScript code.
 * Compiler checks Zero Knowledge properties and issue error message is case of violations.
 * ZK-scoping is optional, it can be used when the user want to ensure Zero Knowledge of
 * specific set of operations.
 * Usually it will require simple restructuring of the code to make the scope body explicit.
 * Invariants checked by the compiler:
 *  - single ZKProof in ErgoTree in a root position
 *  - no boolean operations in the body, because otherwise the result may be disclosed
 *  - all the operations are over SigmaProp values
 *
 * For motivation and details see https://github.com/ScorexFoundation/sigmastate-interpreter/issues/236
 * @since 1.9 
 */
def ZKProof(block: SSigmaProp): Boolean

/** Embedding of Boolean values to SigmaProp values. As an example, this operation allows boolean experesions 
 * to be used as arguments of `atLeast(..., sigmaProp(myCondition), ...)` operation.
 */
def sigmaProp(condition: Boolean): SigmaProp
        
/** Cryptographic hash function Blake2b256 (See scorex.crypto.hash.Blake2b256) */
def blake2b256(input: Coll[Byte]): Coll[Byte]

/** Cryptographic hash function Sha256 (See scorex.crypto.hash.Sha256) */
def sha256(input: Coll[Byte]): Coll[Byte]

/** Create BigInt from a collection of bytes. */
def byteArrayToBigInt(input: Coll[Byte]): BigInt

/** Create Long from a collection of bytes. */
def byteArrayToLong(input: Coll[Byte]): Long  

/** Returns bytes representation of Long value. 
  * @since 2.0
  */
def longToByteArray(input: Long): Coll[Byte]

/**
  * Convert bytes representation of group element (ECPoint) 
  * to a new value of GroupElement (using org.bouncycastle.math.ec.ECCurve.decodePoint())
  * @since 1.9 
  */
def decodePoint(bytes: Coll[Byte]): GroupElement 


/** Returns value of the given type from the context by its tag.*/
def getVar[T](tag: Int): Option[T]

/** Construct a new SigmaProp value representing public key of Diffie Hellman signature protocol. 
  * When executed as part of Sigma protocol allow to provide for a verifier a zero-knowledge proof 
  * of secret knowledge.
  */
def proveDHTuple(g: GroupElement, h: GroupElement, 
                 u: GroupElement, v: GroupElement): SigmaProp
                 
/** Construct a new SigmaProp value representing public key of discrete logarithm signature protocol. 
  * When executed as part of Sigma protocol allow to provide for a verifier a zero-knowledge proof 
  * of secret knowledge.
  */
def proveDlog(value: GroupElement): SigmaProp

/** Predicate function which checks whether a key is in a tree, by using a membership proof. */
def isMember(tree: AvlTree, key: Coll[Byte], proof: Coll[Byte]): Boolean

/**
  * Perform a lookup of key `key` in a tree with root `tree` using proof `proof`.
  * Throws exception if proof is incorrect
  * Return Some(bytes) of leaf with key `key` if it exists
  * Return None if leaf with provided key does not exist.
  */
def treeLookup(tree: AvlTree, key: Coll[Byte], proof: Coll[Byte]): Option[Coll[Byte]]

/** Perform modification of in the tree with root `tree` using proof `proof`.
  * Return Some(newTree) if successfull
  * Return None if the proof was not correct 
  * @since 2.0
  */
def treeModifications(tree: AvlTree, ops: Coll[Byte], proof: Coll[Byte]): Option[AvlTree]

/**
  * Transforms Base58 encoded string litereal into constant of type Coll[Byte].
  * It is a compile-time operation and only string literal (constant) can be its argument.
  */
def fromBase58(input: String): Coll[Byte]

/**
  * Transforms Base64 encoded string litereal into constant of type Coll[Byte].
  * It is a compile-time operation and only string literal (constant) can be its argument.
  */
def fromBase64(input: String): Coll[Byte]

/**
  * It is executed in compile time.
  * The compiler takes Base64 encoding of public key as String literal and create GroupElement constant. 
  * Then the compiler used this constant to construct proveDlog public key out of it.
  * @since 1.9
  */
def PK(input: String): SigmaProp
    
/** Deserializes values from Base58 encoded binary data at compile time */
def deserialize[T](string: String): T

/** Extracts context variable as Coll[Byte], deserializes it to script and then executes this script in the current context. 
  * The original `Coll[Byte]` of the script is available as `getVar[Coll[Byte]](id)`
  * @param id identifier of the context variable
  * @tparam T result type of the deserialized script. 
  * @throws InterpreterException if the actual script type doesn't conform to T
  * @return result of the script execution in the current context
  * @since 2.0
  */
def executeFromVar[T](id: Byte): T

/**
  * Transforms serialized bytes of ErgoTree with segregated constants by replacing constants
  * at given positions with new values. This operation allow to use serialized scripts as
  * pre-defined templates.
  * The typical usage is "check that output box have proposition equal to given script bytes,
  * where minerPk (constants(0)) is replaced with currentMinerPk".
  * Each constant in original scriptBytes have SType serialized before actual data (see ConstantSerializer).
  * During substitution each value from newValues is checked to be an instance of the corresponding type.
  * This means, the constants during substitution cannot change their types.
  *
  * @param scriptBytes serialized ErgoTree with ConstantSegregationFlag set to 1.
  * @param positions zero based indexes in ErgoTree.constants array which should be replaced with new values
  * @param newValues new values to be injected into the corresponding positions in ErgoTree.constants array
  * @return original scriptBytes array where only specified constants are replaced and all other bytes remain exactly the same
  * @since 1.9
  */
def substConstants[T](scriptBytes: Coll[Byte], positions: Coll[Int], newValues: Coll[T]): Coll[Byte]
      
/** Performs outer join operation between left and right collections.
  * This is a restricted version of relational join operation. 
  * It expects `left` and `right` collections have distinct K values in pairs (otherwise exception is thrown). 
  * Under this condition resulting collection has size <= left.size + right.size. 
  * @param l projection function executed for each element of `left`
  * @param r projection function executed for each element of `right`
  * @param inner projection function which is executed for matching items (K, L) and (K, R) with the same K 
  * @return collection of (K, O) pairs, where each key comes form either left or right collection and values are produced by projections
  * @since 2.0
  */
def outerJoin[K, L, R, O]
    (left: Coll[(K, L)], right: Coll[(K, R)])
    (l: (K,L) => O, r: (K,R) => O, inner: (K,L,R) => O): Coll[(K,O)]  // Mainnet
    
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
  val c2 = allOf(Coll(
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
  val c2 = allOf(Coll(
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
The ring is a collection of public keys which correspond to some secret keys (of some parties).
The script below checks that at least one party provided the signature WITHOUT disclosing this party.

```
guard RingSignature(ring: Coll[SigmaProp]) {
  anyOf(ring)
}
```

