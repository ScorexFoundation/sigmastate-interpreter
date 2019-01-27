# SigmaIR Serialization format

This document defines a binary format, which is used to store Sigma scripts in persistent stores, 
to transfer them over wire and to enable cross-platform interoperation.
It organized as the following: first we describe how the types (like `Int`, `Coll[Byte]`, etc.) are serialized, 
then we define serialization of typed data. This will give us a basis to describe serialization of 
Constant nodes of SigmaIR. From that we proceed to serialization of arbitrary SigmaIR trees.

## Type Serialization

### Goal
Currently types are serialized as part of `ExtractRegisterAs`, `ConcreteCollection`, 
`CollectionConstant`, `TaggedVariable`, nodes. The following encoding is designed minimize a number of bytes
required to represent type in serialization format.
In most cases SType value serialises into a single byte.
In the intermediate representation, IR, each type is represented by a tree of nodes where leaves are primitive types 
and other nodes are type constructors.
Simple (sub-optimal) way to serialize type would be to give each primitive type and each type constructor 
a unique type code. Then, to serialize a node, we need to emit its code and then perform recursive descent 
to serialize all children.
However, we use special encoding schema to save bytes for the types that are used more often. 

We assume the most frequently used types are:
- primitive types - Int, Byte, Boolean, BigInt, GroupElement, Box, AvlTree
- Collections of primitive types - `Coll[Byte]` etc
- Options of primitive types - `Option[Int]` etc.
- Nested arrays of primitive types - `Coll[Coll[Int]]` etc.
- Functions of primitive types - `Box => Boolean` etc. 
- First biased pair of types - `(_, Int)` when we know the first component is a primitive type.
- Second biased pair of types - `(Int, _)` when we know the second component is a primitive type.
- Symmetric pair of types - `(Int, Int)` when we know both types are the same

All the types above should be represented in an optimized way (preferable by a single byte).
For other types, we do recursive descent down the type tree as it is defined below.

### Distribution of type codes
The whole space of 256 codes is divided as the following:

Interval | Description
-------|------------
0x00 | special value to represent undefined type (NoType in IR)
0x01 - 0x6F(111) | data types including primitive types, arrays, options aka nullable types, classes (in future), 111 = 255 - 144 different codes
0x70(112) - 0xFF(255) | function types (T1 => T2), 144 = 12 x 12 different codes

### Encoding Data Types

There are 9 different values for primitive types and 2 more are reserved for future extensions.
Each primitive type has id in a range {1,...,11} as the following.

Id    |   Type
------|-------
1     |   Boolean 
2     |   Byte
3     |   Short (16 bit)
4     |   Int (32 bit)
5     |   Long (64 bit)
6     |   BigInt (java.math.BigInteger)
7     |   GroupElement (org.bouncycastle.math.ec.ECPoint)
8     |   SigmaProp 
9     |   reserved for Char 
10    |   reserved for Double 
11    |   reserved 

For each type constructor like Coll or Option we use the encoding schema defined below.
Type constructor has associated _base code_ (e.g. 12 for `Coll[_]`, 24 for `Coll[Coll[_]]` etc. ), which is multiple of 12.
Base code can be added to primitive type id to produce code of constructed type, for example 12 + 1 = 13 is a code of `Coll[Byte]`.
The code of type constructor (12 in this example) is used when type parameter is non-primitive type 
(e.g. `Coll[(Byte, Int)]`), is which case recursive descent is performed.
This encoding allows very simple and quick decoding by using div and mod operations.

The interval of codes for data types is divided as the following:

Interval            | Type constructor   | Description
--------------------|------------------  |------------
0x01 - 0x0B(11)     |                    | primitive types (including 2 reserved)
0x0C(12)            | `Coll[_]`           | Collection of non-primivite types (`Coll[(Int,Boolean)]`)
0x0D(13) - 0x17(23) | `Coll[_]`           | Collection of primitive types (`Coll[Byte]`, `Coll[Int]`, etc.)
0x18(24)            | `Coll[Coll[_]]`      | Nested collection of non-primitive types (`Coll[Coll[(Int,Boolean)]]`)
0x19(25) - 0x23(35) | `Coll[Coll[_]]`      | Nested collection of primitive types (`Coll[Coll[Byte]]`, `Coll[Coll[Int]]`)
0x24(36)            | `Option[_]`        | Option of non-primitive type (`Option[(Int, Byte)]`)
0x25(37) - 0x2F(47) | `Option[_]`        | Option of primitive type (`Option[Int]`)
0x30(48)            | `Option[Coll[_]]` | Option of Coll of non-primitive type (`Option[Coll[(Int, Boolean)]]`)
0x31(49) - 0x3B(59) | `Option[Coll[_]]` | Option of Coll of primitive type (`Option[Coll[Int]]`)
0x3C(60)            | `(_,_)`            | Pair of non-primitive types (`((Int, Byte), (Boolean,Box))`, etc.)
0x3D(61) - 0x47(71) | `(_, Int)`         | Pair of types where first is primitive (`(_, Int)`)
0x48(72)            | `(_,_,_)`          | Triple of types 
0x49(73) - 0x53(83) | `(Int, _)`         | Pair of types where second is primitive (`(Int, _)`)
0x54(84)            | `(_,_,_,_)`        | Quadruple of types 
0x55(85) - 0x5F(95) | `(_, _)`           | Symmetric pair of primitive types (`(Int, Int)`, `(Byte,Byte)`, etc.)
0x60(96)            | `(_,...,_)`        | `Tuple` type with more than 4 items `(Int, Byte, Box, Boolean, Int)`
0x61(97)            |  `Any`             | Any type 
0x62(98)            |  `Unit`            | Unit type
0x63(99)            |  `Box`             | Box type 
0x64(100)           |  `AvlTree`         | AvlTree type 
0x65(101)           |  `Context`         | Context type 
0x65(102)           |  `String`          | String 
0x66(103)           |  `IV`              | TypeIdent 
0x67(104)- 0x6E(110)|                   | reserved for future use 
0x6F(111)           |                    | Reserved for future `Class` type (e.g. user-defined types) 


### Encoding Function Types

We use 12 different values for both domain and range types of functions.
This gives us 12 * 12 = 144 function types in total and allows to represent 11 * 11 = 121 functions over primitive types using just single byte.

Each code F in a range of function types can be represented as

F = D * 12 + R + 112, where D, R in {0,...,11} - indices of domain and range types correspondingly, 
112 - is the first code in an interval of function types. 

If D = 0 then domain type is not primitive and recursive descent is necessary to write/read domain type.

If R = 0 then range type is not primitive and recursive descent is necessary to write/read range type.
 
### Recursive Descent 

When argument of the type constructor is not primitive type we fallback to simple encoding schema.
In such a case we emit special code for the type constructor according to the table above and descend recursively 
to every child node of the type tree.
We do this descend only for those children whose code cannot be embedded in parent code.
For example, serialization of `Coll[(Int,Boolean)]` proceeds as the following:
1) emit 0x0C because element of collection is not primitive 
2) recursively serialize `(Int, Boolean)`
3) emit 0x3D because first item in the pair is primitive
4) recursivley serialize `Boolean`
5) emit 0x02 - the code for primitive type `Boolean`
 
### Examples

Type                 | D   | R   | Bytes             | #Bytes  | Comments
---------------------|-----|-----|-------------------|--------|---------
`Byte`               |     |     |  1                |  1     |
`Coll[Byte]`        |     |     |  12 + 1 = 13      |  1     |
`Coll[Coll[Byte]]` |     |     |  24 + 1 = 25      |  1     | 
`Option[Byte]`       |     |     |  36 + 1 = 37      |  1     | register
`Option[Coll[Byte]]`|     |     |  48 + 1 = 49      |  1     | register
`(Int,Int)`          |     |     |  84 + 3 = 87      |  1     | fold
`Box=>Boolean`       | 7   | 2   |  198 = 7*12+2+112 |  1     | exist, forall
`(Int,Int)=>Int`     | 0   | 3   |  115=0*12+3+112, 87  |  2     |  fold
`(Int,Boolean)`      |     |     |  60 + 3, 2           |  2     |  
`(Int,Box)=>Boolean` | 0   | 2   |  0*12+2+112, 60+3, 7 |  3     | 

## Data and Constant serialization

The contents of a typed data structure can be fully described by a type tree.
For example having a typed data object `d: (Int, Coll[Byte], Boolean)` we can tell that `d` has 3 items, 
the first item contain 64-bit integer, the second - collection of bytes, and the third - logical true/false value. 

To serialize/deserialize typed data we need to know its type descriptor (type tree).
Serialization procedure is recursive over type tree and the corresponding subcomponents of an object.
For primitive types (the leaves of the type tree) the format is fixed. The values of primitive types are serialized 
using predefined function shown in the following table

Value: Type    | Function                      | Format
-------------- |------------------------------ |-------
`x: Byte`      | `byte: Byte => Coll[Byte]`   |  `[x & 0xFF]` - one byte storing value x 
`x: Short`     | `short: Short => Coll[Byte]` |  `[x & 0xFFFF]` - two bytes in big-endian order storing value x
`x: Int`       | `int: Int => Coll[Byte]`     |  `[x & 0xFFFFFFFF]` - four bytes in big-endian order storing value x
`x: Long`      | `long: Int => Coll[Byte]`    |  `[x & 0xFFFFFFFFFFFFFFFF]` - eight bytes in big-endian order storing value x

Thus, serialization format is defined recursively as shown in the following table

Object        | Type             | Format
--------------|------------------|-------
x = 0xXX      | `Byte`           |  `byte(x)` - one byte storing value x 
b = false/true| `Boolean`        |  `if (b) byte(0x01) else byte(0x00)]` - one byte storing 0 or 1
n = 0xXXXXXXXXXXXXXXXX  |  `Int`              |  `[XX,XX,XX,XX,XX,XX,XX,XX]` - big endian 8 bytes
N = new BigInteger()    |  `BigInt`           |  xs = N.toByteArray, `[serialize(xs)]` - serialize as `Coll[Byte]`, see also BigInteger.toByteArray
e = new EcPoint()       |  `GroupElement`     |  `[e.getEncoded(true)]` see also org.bouncycastle.math.ec.EcPoint.getEncoded(true)
box = new ErgoBox()     |  `Box`              |  `[putLong(box.value), putValue(box.proposition), putArray[Any](box.registers), 32, putBytes(box.transactionId), putShort(box.boxId)]`
t = new AvlTree()       |  `AvlTree`          |  `[serialize(t.startingDigest), putInt(t.keyLength), putOpt(t.valueLengthOpt), putOpt(t.maxNumOperations), putOpt(t.maxDeletes)]`
xs = Coll(x1, .., xN)  |  `Coll[T]`      |  `[xs.length & 0xXXXX, serialize(x1), ..., serialize(xN)]` - 2 bytes of length and recursive bytes of all the elements
