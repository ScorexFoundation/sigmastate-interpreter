### Goal
Currently types are serialized as part of `ExtractRegisterAs`, `ConcreteCollection`, 
`CollectionConstant`, `TaggedVariable`, nodes. The encoding is suboptimal.
In most cases SType value should serialise into a single byte.

We assume the most frequently used types are:
- primitive types - Int, Byte, Boolean, BigInt, GroupElement, Box, AvlTree
- Arrays of primitive types - `Array[Byte]` etc
- Options of primitive types - `Option[Int]` etc.
- Nested arrays of primitive types - `Array[Array[Int]]` etc.
- Functions of primitive types - `Box => Boolean` etc. 
- First biased pair of types - `(_, Int)` when we know the first component is a primitive type.
- Second biased pair of types - `(Int, _)` when we know the second component is a primitive type.
- Symmetric pair of types - `(Int, Int)` when we know both types are the same

All the types above should be represented in an optimized way (preferable by a single byte).
For other types, we use recursive descent (decomposition) defined below.

### Distribution of type codes
The whole space of codes is divided as the following:

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
1     |   Byte
2     |   Boolean 
3     |   Int (64 bit)
4     |   BigInt (java.math.BigInteger)
5     |   GroupElement (org.bouncycastle.math.ec.ECPoint)
6     |   AvlTree
7     |   Box
8     |   Unit
9     |   Any
10, 11|   reserved

For each type constructor like Array or Option we use the encoding schema defined below.
Type constructor has associated _base code_ (e.g. 12 for `Array[_]`, 24 for `Array[Array[_]]` etc. ), which is multiple of 12.
Base code can be added to primitive type id to produce code of constructed type, for example 12 + 1 = 13 is a code of `Array[Byte]`.
The code of type constructor (12 in this example) is used when type parameter is non-primitive type 
(e.g. `Array[(Byte, Int)]`), is which case recursive descent is performed.
This encoding allows very simple and quick decoding by using div and mod operations.

The interval of codes for data types is divided as the following:

Interval            | Type constructor | Description
--------------------|------------------|------------
0x01 - 0x0B(11)     |                  | primitive types (including 2 reserved)
0x0C(12) - 0x17(23) | `Array[_]`       | Array types (`Array[Byte]`, `Array[(Int,Boolean)]`)
0x18(24) - 0x23(35) | `Array[Array[_]]`| Nested array types (`Array[Array[Byte]]`, `Array[Array[(Int,Boolean)]]`)
0x24(36) - 0x2F(47) | `Option[_]`      | Option types (`Option[Int]`)
0x30(48) - 0x3B(59) | `Option[Array[_]]` | Option of Array (`Option[Array[Int]]`)
0x3C(60) - 0x47(71) | `(_, Int)`       | First biased pair of types (`(_, Int)`)
0x48(72) - 0x53(83) | `(Int, _)`       | Second biased pair of types (`(Int, _)`)
0x54(84) - 0x5F(95) | `(_, _)`         | Symmetric pair of types (`(Int, Int)`, `(Byte,Byte)`, etc.)
0x60(96) - 0x6B(107)|                  | Reserved for future type constructor (e.g. `Set[_]`)
0x6C(108)           |                  | `Tuple` type with more than 2 items `(Int, Byte, Array[Box])`
0x6C(109)           |                  | Reserved for future `Class` type (e.g. user-defined types)
0x6D(110) - 0x6F(111)|                 | Reserved 


### Encoding Function Types

We use 12 different values for both domain and range types of functions.
This gives us 12 * 12 = 144 function types in total and allows to represent 11 * 11 = 121 functions over primitive types using just single byte.

Each code F in a range of function types can be represented as

F = D * 12 + R + 112, where D, R in {0,...,11} - indices of domain and range types correspondingly, 
112 - is the first code in an interval of function types. 

If D = 0 then domain type is not primitive and recursive descent is necessary to write/read domain type.

If R = 0 then range type is not primitive and recursive descent is necessary to write/read range type.
 
### Examples

Function Type        | D   | R   | Bytes             | #Bytes  | Comments
---------------------|-----|-----|-------------------|--------|---------
`Byte`               |     |     |  1                |  1     |
`Array[Byte]`        |     |     |  12 + 1 = 13      |  1     |
`Array[Array[Byte]]` |     |     |  24 + 1 = 25      |  1     | 
`Option[Byte]`       |     |     |  36 + 1 = 37      |  1     | register
`Option[Array[Byte]]`|     |     |  48 + 1 = 49      |  1     | register
`(Int,Int)`          |     |     |  84 + 3 = 87      |  1     | fold
`Box=>Boolean`       | 7   | 2   |  198 = 7*12+2+112 |  1     | exist, forall
`(Int,Int)=>Int`     | 0   | 3   |  115=0*12+3+112, 87 |  2     |  fold
`(Int,Boolean)`      |     |     |  60 + 3, 2        |  2     |  
`(Int,Box) => Boolean` | 0    | 2    |  0*12+2+112, 60+3, 7 |  3     |  
