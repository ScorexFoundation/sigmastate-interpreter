# ErgoScript Language Description

### Introduction

#### ErgoScript language features

- syntax borrowed from Scala and Kotlin
- standard syntax and semantics for well known constructs (operations, code blocks, if branches etc.)
- high-order with first-class lambdas that are used in collection operations
- call-by-value (eager evaluation)
- statically typed with local type inference
- blocks are expressions 
- semicolon inference in blocks
- type constructors: Tuple, Coll, Option

#### Operations and constructs

- Binary operations: `>, <, >=, <=, +, -, &&, ||, ==, !=, |, *, ^, ++`
- predefined primitives: `blake2b256`, `byteArrayToBigInt`, `proveDlog` etc. 
- val declarations: `val h = blake2b256(pubkey)`
- if-then-else clause: `if (x > 0) 1 else 0`
- collection literals: `Coll(1, 2, 3, 4)`
- generic high-order collection operations: `map`, `fold`, `exists`, `forall`, etc.
- accessing fields of any predefined structured objects: `box.value`
- function invocations (predefined and user defined): `proveDlog(pubkey)` 
- user defined functions: `def isProven(pk: GroupElement) = proveDlog(pk).isProven`
- lambdas and high-order methods: `OUTPUTS.exists { (out: Box) => out.value >= minToRaise }`

#### Data types 

In ErgoScript, everything is an object in the sense that we can call member functions and properties on any variable.
Some of the types can have a special internal representation - for example, numbers and booleans can be
represented as primitive values at runtime - but to the user they look like ordinary classes.
NOTE: in ErgoScript we use *type* and *class* as synonyms, we prefer *type* when talking about primitive values and
*class* when talking about methods.

Type Name        |   Description
-----------------|------------------------------
`Any`            | a supertype of any other type
`Unit`           | a type with a single value `()`
`Boolean`        | a type with two logical values `true` and `false`
`Byte`           | 8 bit signed integer
`Short`          | 16 bit signed integer
`Int`            | 32 bit signed integer
`Long`           | 64 bit signed integer
`BigInt`         | 256 bit signed integer
`SigmaProp`      | a type which represent a logical value which can be be obtained by executing a Sigma protocol with zero-knowledge proof of knowledge
`AvlTree`        | authenticated dynamic dictionary
`GroupElement`   | elliptic curve points
`Box`            | a box containing a monetary value (in NanoErgs), tokens and registers along with a guarding proposition
`Option[T]`      | a container which either have some value of type `T` or none.
`Coll[T]`        | a collection of arbitrary length with all values of type `T` 
`(T1,...,Tn)`    | tuples, a collection of element where T1, ..., Tn can be different types

#### Literal syntax and Constants

Literals are used to introduce values of some types directly in program text like the following examples:
```
 val unit: Unit = ()       // unit constant
 val long: Int = 10        // interger value literal
 val bool: Boolean = true  // logical literal
 val arr = Coll(1, 2, 3)   // constructs a collection with given items
 val str = "abc"           // string of characters 
```
Note that many types don't have literal syntax and their values are introduced 
by applying operations, for example `deserialize` function can be used to introduce
a constant of any type by using Base64 encoded string (See [predefined function](#PredefinedFunctions)).

### Data Types
<a name="DataTypes"></a>

#### Primitive Types

```scala
class Boolean {
  /** Convert true to 1 and false to 0
   * @since 2.0
   */
  def toByte: Byte
}

/** Base supertype for all numeric types. */
class Numeric {
  /** Convert this Numeric value to Byte. 
   * @throws ArithmeticException if overflow happens. 
   */
  def toByte: Byte
  
  /** Convert this Numeric value to Short. 
   * @throws ArithmeticException if overflow happens. 
   */
  def toShort: Short
  
  /** Convert this Numeric value to Int. 
   * @throws ArithmeticException if overflow happens. 
   */
  def toInt: Int
  
  /** Convert this Numeric value to Int. 
   * @throws ArithmeticException if overflow happens. 
   */
  def toLong: Long
  
  /** Convert this Numeric value to Int. */
  def toBigInt: BigInt
  
  /** Returns a big-endian representation of this numeric in a collection of bytes.
   * For example, the long value {@code 0x1213141516171819L} would yield the
   * byte array {@code {0x12, 0x13, 0x14, 0x15, 0x16, 0x17, 0x18, 0x19}}.
   * @since 2.0
   */ 
  def toBytes: Coll[Byte]
  
  /** Returns a big-endian representation of this numeric in a collection of Booleans.
   * Each boolean corresponds to one bit.
   * @since 2.0
   */ 
  def toBits: Coll[Boolean] 
  
   /** Absolute value of this numeric value. 
    * @since 2.0
    */  
  def toAbs: Numeric 
  
  /** Compares this numeric with that numeric for order.  Returns a negative integer, zero, or a positive integer as the
   * `this` is less than, equal to, or greater than `that`.
   */
  def compareTo(that: SNumeric): Int 
}

class Short extends Numeric
class Int extends Numeric
class Long extends Numeric

/**
* All `modQ` operations assume that Q is a global constant (an order of the only one cryptographically strong group
* which is used for all cryptographic operations). 
* So it is globally and implicitly used in all methods.
* */
class BigInt extends Numeric {
  /** Returns this `mod` Q, i.e. remainder of division by Q, where Q is an order of the cryprographic group.
   * @since 2.0
   */  
  def modQ: BigInt 
  /** Adds this number with `other` by module Q.
   * @since 2.0
   */  
  def plusModQ(other: BigInt): BigInt // Testnet2
  /** Subracts this number with `other` by module Q.
   * @since 2.0
   */  
  def minusModQ(other: BigInt): BigInt // Testnet2
  /** Multiply this number with `other` by module Q.
   * @since 2.0
   */  
  def multModQ(other: BigInt): BigInt // Testnet2
   /** Multiply this number with `other` by module Q.
    * @since Mainnet
    */   
  def multInverseModQ: BigInt // ??? @kushti do we need it 
}
```

#### Context Data 

Every script is executed in a context, which is a collection of data available for operations in the script.
The context data is available using variable `CONTEXT` which is of class `Context` defined below.
The following shortcut variables are available in every script to simplify access to the most commonly used context
data.

Variable          |  Type               | Shortcut for ...
------------------|---------------------|----------------------
`HEIGHT`          | `Int`               | `CONTEXT.height`
`SELF`            | `Box`               | `CONTEXT.selfBox` 
`INPUTS`          | `Coll[Box]`         | `CONTEXT.inputs`  
`OUTPUTS`         | `Coll[Box]`         | `CONTEXT.outputs` 

```scala
/**
  * Represents data available in ErgoScript using `CONTEXT` global variable
  * @since 2.0
  */
class Context {
  /** Height (block number) of the block which is currently being validated. */
  def height: Int
  
  /** Box whose proposition is being currently executing */
  def selfBox: Box
  
  /** Zero based index in `inputs` of `selfBox`. */
  def selfBoxIndex: Int
  
  /** A collection of inputs of the current transaction, the transaction where selfBox is one of the inputs. */
  def inputs: Coll[Box]
  
  /** A collection of data inputs of the current transaction. Data inputs are not going to be spent and thus don't
   * participate in transaction validation as `inputs`, but data boxes are available in guarding propositions of 
   * `inputs` and thus can be used in spending logic. 
   * @since 2.0
   */
  def dataInputs: Coll[Box]
  
  /** A collection of outputs of the current transaction. */
  def outputs: Coll[Box]
  
  /** Authenticated dynamic dictionary digest representing Utxo state before current state. */
  def lastBlockUtxoRoot: AvlTree
  
  /**
    * @since 2.0
    */
  def headers: Coll[SHeader] 
  /**
    * @since 2.0
    */
  def preheader: SPreheader  
}

/** Represents data of the block headers available in scripts.
 * @since 2.0
 */
class Header {  
  def version: Byte
  
  /** Bytes representation of ModifierId of the previous block in the blockchain */
  def parentId: Coll[Byte] // 
  
  def ADProofsRoot: Coll[Byte] // Digest32. Can we build AvlTree out of it? 
  def stateRoot: Coll[Byte]  // ADDigest  //33 bytes! extra byte with tree height here!
  def transactionsRoot: Coll[Byte]  // Digest32
  def timestamp: Long
  def nBits: Long  // actually it is unsigned Int 
  def height: Int
  def extensionRoot: Coll[Byte] // Digest32
  def minerPk: GroupElement    // pk
  def powOnetimePk: GroupElement  // w
  def powNonce: Coll[Byte]        // n
  def powDistance: BigInt        // d
}

/** Only header fields that can be predicted by a miner
 * @since 2.0
 */
class PreHeader { // Testnet2
  def version: Byte
  def parentId: Coll[Byte] // ModifierId
  def timestamp: Long
  def nBits: Long  // actually it is unsigned Int 
  def height: Int
  def minerPk: GroupElement
}

class AvlTree {
  /** Returns digest of the state represent by this tree. 
   * @since 2.0
   */
  def digest: Coll[Byte]
}
```

#### Box type

```scala
class Box {
  /** Box monetary value in NanoErg */
  def value: Long 
  
  /** Blake2b256 hash of this box's content, basically equals to `blake2b256(bytes)` */
  def id: Coll[Byte] 

  /** Serialized bytes of guarding script, which should be evaluated to true in order to open this box. */
  def propositionBytes: Coll[Byte] 
  
  /** Serialized bytes of this box's content, including proposition bytes. */
  def bytes: Coll[Byte] 
  
  /** Serialized bytes of this box's content, excluding transactionId and index of output. */
  def bytesWithoutRef: Coll[Byte]
    
  /** If `tx` is a transaction which generated this box, then `creationInfo._1` is a height of the tx's block.
   *  The `creationInfo._2` is a serialized transaction identifier followed by box index in the transaction outputs. 
   */
  def creationInfo: (Int, Coll[Byte]) 
  
  /** Synonym of R2 obligatory register
   * @since 2.0
   */
  def tokens: Coll[(Coll[Byte], Long)] 
  
  /** Extracts register by id and type. 
   * @param regId zero-based identifier of the register.
   * @tparam T expected type of the register.
   * @return Some(value) if the register is defined and has given type.
   *         None otherwise
   * @since 2.0
   */
  def getReg[T](regId: Int): Option[T]

  /** Extracts register as Coll[Byte], deserializes it to script and then executes this script in the current context. 
    * The original Coll[Byte] of the script is available as getReg[Coll[Byte]](id)
    * @param regId identifier of the register 
    * @tparam T result type of the deserialized script. 
    * @throws InterpreterException if the actual script type doesn't conform to T
    * @return result of the script execution in the current context
    * @since Mainnet
    */
  def executeFromRegister[T](regId: Byte): T
}
```

Besides properties, every box can have up to 10 numbered registers.
The following syntax is supported to access registers on box objects:
```
box.getReg[Int](3).get     // access R3 register, cast its value to Int and return it
box.getReg[Int](3).isDefined  // check that value of R3  is defined and has type Int
box.getReg[Int](3).isEmpty    // check that value of R3 is either undefined or have not Int type
box.getReg[Int](3).getOrElse(def) // access R3 value if defined, otherwise return def
```

#### GroupElement
```scala
class GroupElement {
  // ...
  /**
  * // this should replace the currently used ^ 
   * @since 2.0
   */
  def exp(n: BigInt): GroupElement // Testnet2 
}
```

#### AvlTree

```scala
class AvlTree {
  /**
   * @since 2.0
   */
  def digest: Coll[Byte]
}
```

#### Option[T]

```scala
/** Represents optional values. Instances of `Option`
 *  are either an instance of `Some(x)` or the value `None`.
 */
class Option[A] {
  /** Returns true if the option is None, false otherwise. 
   */  
  def isEmpty: Boolean;
  
  /** Returns true if the option is an instance of Some(value), false otherwise. 
   */
  def isDefined: Boolean;
  
  /** Returns the option's value if the option is nonempty, otherwise
   * return the result of evaluating `default`.
   *
   * @param default  the default expression, which is evaluated only if option is None.
   */
  def getOrElse[B](default: => B): B
  
  /** Returns the option's value.
   *  @note The option must be nonempty.
   *  @throws InterpreterException if the option is empty.
   */
  def get: A

  /** Returns a singleton collection containing the $option's value
   * if it is nonempty, or the empty collection if the $option is empty.
   * @since  2.0
   */
  def toColl: Coll[A]
  
  /** Returns a Some containing the result of applying $f to this option's
   * value if this option is nonempty.
   * Otherwise return None.
   *
   * @note This is similar to `flatMap` except here, $f does not need to wrap its result in an $option.
   *
   * @param  f   the function to apply
   * @since  2.0
   * @see flatMap
   */
  def map[B](f: A => B): Option[B]

  
  /** Returns this option if it is nonempty '''and''' applying the predicate $p to
   * this option's value returns true. Otherwise, return $none.
   *
   * @param  p   the predicate used for testing.
   * @since  2.0
   */
  def filter(p: A => Boolean): Option[A]
  
  /** Returns the result of applying $f to this option's value if
   * this option is nonempty.
   * Returns None if this option is empty.
   * Slightly different from `map` in that $f is expected to
   * return an option (which could be None).
   *
   *  @param  f   the function to apply
   *  @see map
   *  @since 2.0
   */
  def flatMap[B](f: A => Option[B]): Option[B]
}
```

#### Coll[T]

```scala
/** Indexed (zero-based) collection of elements of type `A` 
  * @define Coll `Coll`
  * @define coll collection
  * @tparam A the collection element type
  */
class Coll[A] {
  /** The length of the collection */
  def length: Int
  
  /** The element at given index.
   *  Indices start at `0`; `xs.apply(0)` is the first element of collection `xs`.
   *  Note the indexing syntax `xs(i)` is a shorthand for `xs.apply(i)`.
   *
   *  @param    i   the index
   *  @return       the element at the given index
   *  @throws       ArrayIndexOutOfBoundsException if `i < 0` or `length <= i`
   */
  def apply(i: Int): A
  
  /** The element of the collection or default value. 
   * If an index is out of bounds (`i < 0 || i >= length`) then `default` value is returned.
   *  @param    i   the index
   *  @return       the element at the given index or default value if index is out or bounds
   *  @since 2.0
   */
  def getOrElse(i: Int, default: A): A
  
  /** Builds a new collection by applying a function to all elements of this collection.
   *
   *  @param f      the function to apply to each element.
   *  @tparam B     the element type of the returned collection.
   *  @return       a new collection of type `Coll[B]` resulting from applying the given function
   *                `f` to each element of this collection and collecting the results.
   */
  def map[B](f: A => B): Coll[B]

  /** For this collection (x0, ..., xN) and other collection (y0, ..., yM)
   * produces a collection ((x0, y0), ..., (xK, yK)) where K = min(N, M) 
   * @since 2.0
   */
  def zip[B](ys: Coll[B]): PairColl[A, B]

  /** Tests whether a predicate holds for at least one element of this collection.
   *  @param   p     the predicate used to test elements.
   *  @return        `true` if the given predicate `p` is satisfied by at least one element of this collection, otherwise `false`
   */
  def exists(p: A => Boolean): Boolean
  
  /** Tests whether a predicate holds for all elements of this collection.
   *  @param   p   the predicate used to test elements.
   *  @return      `true` if this collection is empty or the given predicate `p`
   *               holds for all elements of this collection, otherwise `false`.
   */
  def forall(p: A => Boolean): Boolean
  
  /** Selects all elements of this collection which satisfy a predicate.
   *  @param p     the predicate used to test elements.
   *  @return      a new collection consisting of all elements of this collection that satisfy the given
   *               predicate `p`. The order of the elements is preserved.
   *  @since 2.0
   */
  def filter(p: A => Boolean): Coll[A]
  
  /** Applies a binary operator to a start value and all elements of this collection,
   *  going left to right.
   *
   *  @param   z    the start value.
   *  @param   op   the binary operator.
   *  @tparam  B    the result type of the binary operator.
   *  @return  the result of inserting `op` between consecutive elements of this collection,
   *           going left to right with the start value `z` on the left:
   *           {{{
   *             op(...op(z, x_1), x_2, ..., x_n)
   *           }}}
   *           where `x,,1,,, ..., x,,n,,` are the elements of this collection.
   *           Returns `z` if this collection is empty.
   */
  def foldLeft[B](z: B)(op: (B, A) => B): B

  /** Produces the range of all indices of this collection [0 .. size-1] 
   *  @since 2.0
   */
  def indices: Coll[Int]

  /**
    * Builds a new collection by applying a function to all elements of this collection
    * and using the elements of the resulting collections.
    *
    * @param f the function to apply to each element.
    * @tparam B the element type of the returned collection.
    * @return a new collection of type `Coll[B]` resulting from applying the given collection-valued function
    *         `f` to each element of this collection and concatenating the results.
    * @since 2.0
    */
  def flatMap[B](f: A => Coll[B]): Coll[B]

  /** Computes length of longest segment whose elements all satisfy some predicate.
    *
    *  @param   p     the predicate used to test elements.
    *  @param   from  the index where the search starts.
    *  @return  the length of the longest segment of this collection starting from index `from`
    *           such that every element of the segment satisfies the predicate `p`.
    *  @since 2.0
    */
  def segmentLength(p: A => Boolean, from: Int): Int

  /** Finds the first element of the $coll satisfying a predicate, if any.
    *
    *  @param p       the predicate used to test elements.
    *  @return        an option value containing the first element in the $coll
    *                 that satisfies `p`, or `None` if none exists.
    */
  def find(p: A => Boolean): Option[A]
  
  /** Finds index of the first element satisfying some predicate after or at some start index.
    *
    *  @param   p     the predicate used to test elements.
    *  @param   from   the start index
    *  @return  the index `>= from` of the first element of this collection that satisfies the predicate `p`,
    *           or `-1`, if none exists.
    *  @since 2.0
    */
  def indexWhere(p: A => Boolean, from: Int): Int

  /** Finds index of last element satisfying some predicate before or at given end index.
    *
    *  @param   p     the predicate used to test elements.
    *  @return  the index `<= end` of the last element of this collection that satisfies the predicate `p`,
    *           or `-1`, if none exists.
    *  @since 2.0
    */
  def lastIndexWhere(p: A => Boolean, end: Int): Int


  /** Partitions this collection in two collectionss according to a predicate.
    *
    *  @param      pred the predicate on which to partition.
    *  @return     a pair of collections: the first collection consists of all elements that
    *              satisfy the predicate `p` and the second collection consists of all elements
    *              that don't. The relative order of the elements in the resulting collections
    *              will BE preserved (this is different from Scala's version of this method).
    *  @since 2.0
    */
  def partition(pred: A => Boolean): (Coll[A], Coll[A])

  /** Produces a new collection where a slice of elements in this collection is replaced by another sequence.
    *
    *  @param  from     the index of the first replaced element
    *  @param  patch    the replacement sequence
    *  @param  replaced the number of elements to drop in the original collection
    *  @return          a new collection consisting of all elements of this collection
    *                   except that `replaced` elements starting from `from` are replaced by `patch`.
    *  @since 2.0
    */
  def patch(from: Int, patch: Coll[A], replaced: Int): Coll[A]

  /** A copy of this collection with one single replaced element.
    *  @param  index  the position of the replacement
    *  @param  elem   the replacing element
    *  @return a new collection which is a copy of this collection with the element at position `index` replaced by `elem`.
    *  @throws IndexOutOfBoundsException if `index` does not satisfy `0 <= index < length`.
    *  @since 2.0
    */
  def updated(index: Int, elem: A): Coll[A]

  /** Returns a copy of this collection where elements at `indexes` are replaced with `values`. 
   *  @since 2.0
   */
  def updateMany(indexes: Coll[Int], values: Coll[A]): Coll[A]

  /** Apply m for each element of this collection, group by key and reduce each group using r.
   *  @returns one item for each group in a new collection of (K,V) pairs. 
   *  @since 2.0
   */
  def mapReduce[K, V](m: A => (K,V), r: (V,V) => V): Coll[(K,V)]

  /** Partitions this $coll into a map of ${coll}s according to some discriminator function.
    *
    *  @param key   the discriminator function.
    *  @tparam K    the type of keys returned by the discriminator function.
    *  @return      A map from keys to ${coll}s such that the following invariant holds:
    *               {{{
    *                 (xs groupBy key)(k) = xs filter (x => key(x) == k)
    *               }}}
    *               That is, every key `k` is bound to a $coll of those elements `x`
    *               for which `key(x)` equals `k`.
    */
  def groupBy[K: RType](key: A => K): Coll[(K, Coll[A])]

  /** Partitions this $coll into a map of ${coll}s according to some discriminator function.
    * Additionally projecting each element to a new value.
    *
    *  @param key   the discriminator function.
    *  @param proj  projection function to produce new value for each element of this $coll
    *  @tparam K    the type of keys returned by the discriminator function.
    *  @tparam V    the type of values returned by the projection function.
    *  @return      A map from keys to ${coll}s such that the following invariant holds:
    *               {{{
    *                 (xs groupByProjecting (key, proj))(k) = xs filter (x => key(x) == k).map(proj)
    *               }}}
    *               That is, every key `k` is bound to projections of those elements `x`
    *               for which `key(x)` equals `k`.
    */
  def groupByProjecting[K: RType, V: RType](key: A => K, proj: A => V): Coll[(K, Coll[V])]

  /** Produces a new collection which contains all distinct elements of this collection and also all elements of
   *  a given collection that are not in this collection.
   *  This is order preserving operation considering only first occurrences of each distinct elements.
   *  Any collection `xs` can be transformed to a sequence with distinct elements by using xs.unionSet(Coll()).
   *
   *  NOTE: Use append if you don't need set semantics.
   *
   *  @param that   the collection to add.
   *  @since 2.0
   */
  def unionSets(that: Coll[A]): Coll[A]

  /** Computes the multiset difference between this collection and another sequence.
   *
   *  @param that   the sequence of elements to remove
   *  @tparam B     the element type of the returned collection.
   *  @return       a new collection which contains all elements of this collection
   *                except some of occurrences of elements that also appear in `that`.
   *                If an element value `x` appears
   *                ''n'' times in `that`, then the first ''n'' occurrences of `x` will not form
   *                part of the result, but any following occurrences will.
   *  @since 2.0
   */
  def diff(that: Coll[A]): Coll[A]

  /** Computes the multiset intersection between this collection and another sequence.
   *  @param that   the sequence of elements to intersect with.
   *  @return       a new collection which contains all elements of this collection
   *                which also appear in `that`.
   *                If an element value `x` appears
   *                ''n'' times in `that`, then the first ''n'' occurrences of `x` will be retained
   *                in the result, but any following occurrences will be omitted.
   *  @since 2.0
   */
  def intersect(that: Coll[A]): Coll[A]

  /** Folding through all elements of this $coll starting from m.zero and applying m.plus to accumulate
    * resulting value.
    *
    * @param m monoid object to use for summation
    * @return  result of the following operations (m.zero `m.plus` x1 `m.plus` x2 `m.plus` ... xN)
    * @since 2.0
    */
  def sum(m: Monoid[A]): A
  
  /** Selects an interval of elements.  The returned collection is made up
   *  of all elements `x` which satisfy the invariant:
   *  {{{
   *    from <= indexOf(x) < until
   *  }}}
   *  @param from   the lowest index to include from this collection.
   *  @param until  the lowest index to EXCLUDE from this collection.
   */
  def slice(from: Int, until: Int): Coll[A]
  
  /** Puts the elements of other collection after the elements of this collection (concatenation of 2 collections)
   */
  def append(other: Coll[A]): Coll[A]
  
  /** Returns the length of the longest prefix whose elements all satisfy some predicate.
   *  @param   p     the predicate used to test elements.
   *  @return  the length of the longest prefix of this collection
   *           such that every element of the segment satisfies the predicate `p`.
   *  @since 2.0
   */
  def prefixLength(p: A => Boolean): Int

  /** Finds index of first occurrence of some value in this collection after or at some start index.
   *  @param   elem   the element value to search for.
   *  @param   from   the start index
   *  @return  the index `>= from` of the first element of this collection that is equal (as determined by `==`)
   *           to `elem`, or `-1`, if none exists.
   *  @since 2.0
   */
  def indexOf(elem: A, from: Int): Int

  /** Finds index of last occurrence of some value in this collection before or at a given end index.
   *
   *  @param   elem   the element value to search for.
   *  @param   end    the end index.
   *  @return  the index `<= end` of the last element of this collection that is equal (as determined by `==`)
   *           to `elem`, or `-1`, if none exists.
   *  @since 2.0
   */
  def lastIndexOf(elem: A, end: Int): Int

  /** Finds the first element of the collection satisfying a predicate, if any.
   *  @param p       the predicate used to test elements.
   *  @return        an option value containing the first element in the collection
   *                 that satisfies `p`, or `None` if none exists.
   *  @since 2.0
   */
  def find(p: A => Boolean): Option[A]

  /** Builds a new collection from this collection without any duplicate elements.
   *  @return  A new collection which contains the first occurrence of every element of this collection.
   *  @since 2.0
   */
  def distinct: Coll[A]

  /** Tests whether this collection contains the given sequence at a given index.
   *
   * '''Note''': If the both the receiver object `this` and the argument
   * `that` are infinite sequences this method may not terminate.
   *
   * @param  that    the sequence to test
   * @param  offset  the index where the sequence is searched.
   * @return `true` if the sequence `that` is contained in this collection at
   *         index `offset`, otherwise `false`.
   * @since 2.0
   */
  def startsWith(that: Coll[A], offset: Int): Boolean

  /** Tests whether this collection ends with the given collection.
    *  @param  that   the collection to test
    *  @return `true` if this collection has `that` as a suffix, `false` otherwise.
    *  @since 2.0
    */
  def endsWith(that: Coll[A]): Boolean

}
```

Each item can be accessed by constant index, for example:
```
val myOutput = OUTPUTS(0)
val myInput = INPUTS(0)
```

Any collection have a `length` property which returns number of elements in a collection. 

```
val l = OUTPUTS.length
```

The following script check an existence of some element in the collection satisfying some
predicate (condition)

```
val ok = OUTPUTS.exists { (box: Box) => box.value > 1000 }
``` 

### Predefined global functions
<a name="PredefinedFunctions"></a>

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
  * The compiler takes Base58 encoding of public key as String literal and create GroupElement constant. 
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

See [white paper for example](wpaper/sigma.tex)
