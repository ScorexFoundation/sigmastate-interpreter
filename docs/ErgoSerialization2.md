# Ergo serialization format specification

## Goal and structure
The goal of this paper is to summarize and formalize the serialization procedure of the ``ErgoTree`` object. During the serialization objects of different types appear. All the type kinds can be considered separately and therefore the document contains correspondent sections  dedicated to particular kinds. Further in each section for each type (or serialization case) the particular format is given. 

It can be a case when serializing compound object that it contains structures of undetermined length. So we need the specification format which support this case.

## Format
We use the following format to describe a particular serialization case:
 ``S => {Field(1), Field(2),...}``, where ``S`` is currently being described type or serialization case and the content inside the braces are fields placed one by one. 

Each ``Field(i)`` has its own format ``Field = name: tr(1) >> tr(2) >> ... >> S``, where ``name`` is the both the field name and some property of the serialized object (some exceptions here may occur, we will note it). `tr(i)` is the transformer to be used on the ``name`` property before piping further. ``S`` is already defined serializer or serializers family (if cannot be determined). Note that ``S`` can be recursively defined. 

TODO: describe * pattern`

Each serializer is included in some family as written before. When we need to use the particular serializer from the family    
we use the notation ``F(S)`` with ``F`` standing for family name, and ``S`` for particular serializer inside the family.

Serializer can potentially have some cases when depending on the property values the object is stored differently. We denote this as ``[condition] | S => ...`` where ``condition`` presents the case when ``S`` is used as written after ``=>`` sign.  ``[_]`` condition pattern stands for all other cases and is used as the last one.

When serializer has exceptional behavior and requires some constrains we denote it in ``[assert]`` block below the main definition.

In some cases we provide the particular Scala code for transformers or conditions below the serializer definition.

Concluding, the final format is:
``[condition] | S => {name1: tr(1,1) >> tr(1,2) >> ... >> [condition] S(1); ...} ``. 

In some cases we omit the ``name`` part, then the transformers are applied to the whole serialized object. We can also omit the braces in the case when only one (unnamed) field is being serialized. 

Further each section name also corresponds to serializers family.

## Basic
The serialization is performed by storing the correspondent bytes in ``ByteWriter`` object inherited from ``ByteArrayBuilder`` java class which has besides others the following basic operations:
``append (x: Byte)``
``append (x: Short)``  
``append (x: Array [Byte])``
``append (x: Boolean)``
which append the correspondent to internal storage in polymorphic way. All of these operations we note as ``APPEND`` serializer. The appending procedure is developed inside the standard library and is beyond the scope of this document. 
We also use the ``NOTHING`` serializer which does nothing for notation convenience.

## Native
Native types are types given by the Scala standard library and are serialized as follows:
1. ``Byte => APPEND``
2. ``UByte => Byte``
``[assert] (_ >= 0 && _ <= 0xFF)``
4. ``UInt => ULong``
``[assert] (_ >= 0 && _ <= 0xFFFFFFFFL)``
5. ``Boolean => APPEND``
6. ``Array [Byte] => APPEND``
7. ``Array [Boolean] => (toBytes >> Array [Byte])``
where ``toBytes`` is defined as 
```scala
	def putBits(xs: Array[Boolean]): this.type = { 
	  if (xs.isEmpty) return this
	  val bitSet = new BitSet(xs.length)  
	  xs.zipWithIndex.foreach { case (bool, i) => bitSet.set(i, bool)}  
	  val bytes = Arrays.copyOf(bitSet.toByteArray, (xs.length + 7) / 8)  
	  b.append(bytes)  
	  this  
	}
```
8. ``UShort => UInt``
``[assert] (_ >= 0 && _ <= 0xFFFF)``
9. ``Long => (ZigZagLong >> ULong)``
where ``ZigZagLong (x: Long) : Long`` is defined following the [code](http://github.com/google/protobuf/blob/a7252bf42df8f0841cf3a0c85fdbf1a5172adecb/java/core/src/main/java/com/google/protobuf/CodedOutputStream.java#L949). 
10. ``Int => (ZigZagInt >> ULong)``
where ``ZigZagInt (x: Int) : Int`` is defined following the [code](http://github.com/google/protobuf/blob/a7252bf42df8f0841cf3a0c85fdbf1a5172adecb/java/core/src/main/java/com/google/protobuf/CodedOutputStream.java#L934). 
11. ``ULong => (VLQEnc >> APPEND)``
where ``VLQEnc (x: Long): Array[Byte]`` is defined following the [code](http://github.com/google/protobuf/blob/a7252bf42df8f0841cf3a0c85fdbf1a5172adecb/java/core/src/main/java/com/google/protobuf/CodedOutputStream.java#L1387). 
12. ``Short => APPEND``
13. ``Option [X]``
11.1 ``[None] | Option X => {header: 0 >> Byte}``
11.2 ``[Some v] | Option X => {header: 1 >> Byte; v: Serializer}``
with arbitrary externally given serializer for ``v``.

## Data
The current section is dedicated for S-typed objects data serialization. Some S-types have implicit coercion to correspondent native types performed by ``v.asInstanceOf[T]`` call with ``v`` of some S-type and ``T`` is coercible (nested) native type.   The implicit conversion transformation is omitted in notation.
1. ``SUnit => NOTHING``
2. ``SBoolean => Native (Boolean)``
3. ``SByte => Native (Byte)``
4. ``SShort => Native (Short)``
5. ``SInt => Native (Int)``
6. ``SLong => Native (Long)``
7.  ``SString => {length: Native (UInt);``
	 $\qquad$$\qquad$$\quad$``bytes: encode >> Native (Array [Byte])}``
	 where ``length`` stands for the ``Array.length()`` return value for the array produced by ``encode`` transformer which is defined as ``String.getBytes(StandardCharsets.UTF_8)`` for serialized string.
8. ``SBigInt => {length: Native (UShort);`` 
$\qquad$$\qquad$$\quad$``bytes: toByteArray >> Array [Byte]}``
where ``length`` stands for the ``Array.length()`` return value for the array produced by ``toByteArray`` transformer which is defined as ``BigInteger.toByteArray()`` for serialized string.
9. ``SGroupElement => Body (GroupElement)``
Here we refer to the ``Body`` serializers family defined below and the coercion ``SGroupElement -> EcPointType``.
10. ``SSigmaProp => Value``
Here we refer to the ``Value`` serializers family defined below and the coercion ``SSigmaProp -> SigmaBoolean``.
11. ``SBox => Body (ErgoBox)``
Here we refer to the ``Body`` serializers family defined below and the coercion ``SBox -> ErgoBox``.
12. ``SAvlTree => Body (AvlTreeData)``
Here we refer to the ``Body`` serializers family defined below and the coercion ``SAvlTree -> AvlTreeData``.
13. ``SCollectionType [X]``
13.1 ``[X == SBoolean] |`` with coercion ``SCollectionType[X] -> Array[T#WrappedType]``.
$\qquad$``SCollectionType X => {length: Native (UShort);`` 
$\qquad$$\qquad$$\qquad$$\qquad$$\qquad$$\quad$``bits: Native (Array [Boolean])}``
Here ``length`` is the collection length defined by ``Array.length()`` for coerced object and the explicit coercion ``SCollectionType[SBoolean] -> Array[Boolean]`` to get ``bits``.
13.2 ``[X == SByte] |`` 
$\qquad$``SCollectionType X => {length: Native (UShort); ``
$\qquad$$\qquad$$\qquad$$\qquad$$\qquad$$\quad$``bytes: Native (Array [Byte])}``
Here ``length`` is the collection length defined by ``Array.length()`` for coerced object and the explicit coercion ``SCollectionType[SByte] -> Array[Byte]`` to get ``bytes``.
13.3 ``[_] |``
$\qquad$`` SCollectionType => {length: Native (UShort); ``
$\qquad$$\qquad$$\qquad$$\qquad$$\qquad$``elems: Data*}``
Here ``length`` is the collection length defined by ``Array.length()`` for the object produced by the general coercion. The serialization for  ``elems`` is given by mapping each element with correspondent ``Data`` family serializer.
14. ``STuple => Data*`` with coercion  ``STuple -> Array[Any]``
``[assert] (arr.length == items.length)``
``[assert] (arr.length <= 0xFFFF)``  
where ``arr`` is the coerced array and ``items`` is the array of the serialized tuple types.

## Type
Type serialization is given in another [document](https://github.com/ScorexFoundation/sigmastate-interpreter/blob/master/docs/TypeSerialization.md).

## Value
The ``Value`` trait represent the wide range of objects serialized by three variants. ``Value`` trait extends ``SigmaNode`` trait which is sase type for all AST nodes of sigma language. Every value represents an operation and that operation can be associated with a function type,  describing functional meaning of the operation, kind of operation signature. The serialization behavior depends on kinds of serializable value and its ``opCode`` property. The first differentiation reflects the serialization cases in this section and ``opCode`` helps to choose the correspondent serializer from the table to serialize the ``Body`` of value. 

To realize which serialization case should be chosen we firstly analyze the result of ``serializable`` function which is defined as follows:
```scala
def serializable(v: Value[SType]): Value[SType] = v match {  
  case upcast: Upcast[SType, _]@unchecked =>  
    upcast.input  
  case _ => v  
}
```
Then if the result is of ``Constant [SType]`` type we see if we have the ``constantExtractionStore`` of the ``ByteWriter`` object field set to not ``None`` value. Note that having ``constantStore`` is the property of the whole serialization procedure specified by ``ByteWriter`` object rather than a particular ``Value``.

So we define the following serialization cases:

1.  ``Value``
1.1 ``[Non-constant] | Value => {opCode: Native (Byte);`` 
$\qquad$$\qquad$$\qquad$$\qquad$$\qquad$$\qquad$$\quad$``body: Body}``
1.2 ``[Constant with store] | Value => {ConstantPlaceholderIndexCode: Native (Byte);``
$\qquad$$\qquad$$\qquad$$\qquad$$\qquad$$\qquad$$\quad$$\quad$``constantPlaceholder: Body (ConstantPlaceholder)}``
1.3 ``[Constant without store] | Value => Body (Constant)``
2. ``Values = {length: UInt;`` 
$\qquad$$\qquad$``values: Value*}``
This serialization is used to store multiple ``Value`` objects with ``length = values.length()`` and ``Value`` is mapped to each member of the serialized ``Seq [Value [T]]``.

To serialize ``Body`` we need to choose  the correspondent serializer from the table based on its ``opCode`` which is given by the following reference code:
```scala
override def getSerializer(opCode: Tag): ValueSerializer[_ <: Value[SType]] = {  
  val serializer = serializers.get(opCode)  
  if (serializer == null)  
    throw new InvalidOpCode(s"Cannot find serializer for Value with opCode = LastConstantCode + ${opCode.toUByte - LastConstantCode}")  
  serializer  
}
```
with ``serializers`` hash-table in essential defined as pairs ``(opCode, Serializer)``. We give the ``opCode -> Serializer``  correspondence in the next section (which should not be treated as serializers family therefore).

### Table for ``serializers``
| No |``opCode`` name| ``opCode`` values[*](#shifted) |``Body`` case serializer |
|--|--|--|--|
|1|``ConstantCode``  | ``0``[**](#notShifted)| ``Constant`` |
|2|``ConstantPlaceholderIndexCode`` |``3``| ``ConstantPlaceholder`` |
|3| ``TupleCode`` | ``22`` | ``Tuple`` |
|4| ``SelectFieldCode`` |``28``| ``SelectField`` |
|5| ``LtCode``, ``LeCode``, ``GtCode``, <br> ``GeCode``, ``EqCode``, ``NeqCode``, <br> ``BinAndCode``, ``BinOrCode`` | ``31`` &#247; ``36``, ``38``, ``39``| ``Relation2`` |
|6| ``TreeLookupCode``, ``TreeModificationsCode``, ``IfCode`` | ``70``, ``71``, ``37``| ``Quadruple`` |
|7| ``MinusCode``, ``PlusCode``, ``XorCode``, <br> ``MultiplyCode``, ``DivisionCode``, ``ModuloCode``, <br> ``ExponentiateCode``, ``MultiplyGroupCode``, <br> ``MinCode``, ``MaxCode`` | ``(41``&#247;``50)``| ``TwoArguments``|
|8| ``ProveDiffieHellmanTupleCode`` | ``94`` | ``ProveDHTuple`` |
|9| ``ProveDlogCode`` | ``93`` | ``ProveDlog`` |
|10| ``TrueCode``, ``FalseCode``, <br> ``HeightCode``, ``InputsCode``, ``OutputsCode``,  <br> ``LastBlockUtxoRootHashCode``, ``SelfCode``, <br> ``MinerPubkeyCode``, ``GroupGeneratorCode``,  <br> ``TrivialPropFalseCode``, ``TrivialPropTrueCode``| ``15``,``16``,``51``&#247;``55``,<br>``60``, ``18``, ``98``, ``99`` | ``CaseObject`` |
|11| ``SigmaPropIsProvenCode`` | ``95``| ``SigmaPropIsProven`` |
|12| ``SigmaPropBytesCode`` | ``96`` | ``SigmaPropBytes`` |
|13| ``ConcreteCollectionBooleanConstantCode``| ``21`` | ``ConcreteCollectionBooleanConstant``|
|14| ``ConcreteCollectionCode`` | ``19`` | ``ConcreteCollection`` |
|15| ``AndCode``, ``OrCode`` | ``38,39``| ``LogicalTransformer`` |
|16| ``TaggedVariableCode`` | ``1``| ``TaggedVariable`` |
|17| ``GetVarCode`` | ``115`` | ``GetVar`` |
|18| ``MapCollectionCode`` | ``61`` | ``MapCollection``|
|19| ``ExistsCode``, ``ForAllCode`` | ``62``, ``63``| ``BooleanTransformer``|
|20| ``FoldCode`` | ``64`` | ``Fold`` |
|21| ``SizeOfCode``, ``ExtractAmountCode``, ``ExtractScriptBytesCode``, <br> ``ExtractBytesCode``, ``ExtractBytesWithNoRefCode``, ``ExtractIdCode``, <br> ``ExtractCreationInfoCode``, ``LongToByteArrayCode``, <br> ``ByteArrayToBigIntCode``, ``CalcBlake2b256Code``, ``CalcSha256Code``, <br> ``DecodePointCode``, ``OptionGetCode``, ``OptionIsDefinedCode`` | ``65``, ``81``&#247;``85``, ``87``, ``10``, ``11``, ``91``, ``92``, ``126``, ``116``, ``118``| ``SimpleTransformer``|
|22| ``OptionGetOrElseCode`` | ``117`` | ``OptionGetOrElse`` |
|23| ``DeserializeContextCode`` | ``100`` | ``DeserializeContext``|
|24| ``DeserializeRegisterCode``| ``101`` | ``DeserializeRegister``|
|25| ``ExtractRegisterAs``| ``86`` | ``ExtractRegisterAs``|
|26| ``FilterCode`` | ``69`` | ``Filter``|
|27| ``SliceCode`` | ``68`` | ``Slice``|
|28| ``AtLeastCode`` | ``40`` | ``AtLeast``|
|29| ``ByIndexCode`` | ``66`` | ``ByIndex``|
|30| ``AppendCode`` | ``67`` | ``Append`` |
|31| ``DowncastCode``, ``UpcastCode`` | ``13``, ``14``| ``NumericCast`` |
|32| ``ValDefCode``, ``FunDefCode``| ``102``, ``103`` | ``ValDef``|
|33| ``BlockValueCode``| ``104`` | ``BlockValue``|
|34| ``ValUseCode`` | ``2`` | ``ValUse``|
|35| ``FuncValueCode`` | ``105`` | ``FuncValue``|
|36| ``FuncApplyCode`` | ``106`` | ``Apply``|
|37| ``PropertyCallCode``, ``MethodCallCode`` | ``107``, ``108``| ``MethodCall``|
|38| ``SigmaAndCode``, ``SigmaOrCode`` | ``122``, ``123`` | ``SigmaTransformer``|
|39| ``BoolToSigmaPropCode`` | ``97`` | ``BoolToSigmaProp``|
|40| ``ModQCode`` | ``119`` | ``ModQ``|
|41| ``PlusModQCode``, ``MinusModQCode`` | ``120``, ``121`` | ``ModQArithOp`` |
|42| ``SubstConstantsCode`` | ``4`` | ``SubstConstants``|
 
##### <a name="shifted"> </a> * All the numeric values  presented in the table should be added to constant ``LastConstantCode`` to get the final ``opCode`` value
##### <a name="notShifted">  </a> ** Besides of this, the ``ConstantCode=0`` literally


## Body
1. Constant => {tpe: Type; value: Data}
2. ConstantPlaceholder => {id: UInt}
3. Tuple => {length: UByte; items: Value*}
4. SelectField => {input: Value; fieldIndex: Byte}
5. [Not BooleanConstants] | Relation2 => {left: Value; right: Value}
   [BooleanConstants] | Relation2 => {ConcreteCollectionBooleanConstantCode: Byte; LeftRightBits: Bits}
6. Quadruple => {first: Value; second: Value; third: Value}
7. TwoArguments => {left: Value; right: Value}
8. [SGroupElementConstants] | ProveDHTuple => {constCodePrefix: Byte; SGroupElementType: Type;
                                            gv_data: Data(SGroupElement); hv_data: Data(SGroupElement);
					    uv_data: Data(SGroupElement); vv_data: Data(SGroupElement)}
   [Not SGroupElementConstants] | ProveDHTuple => {gv: Value(SGroupElement); hv: Value(SGroupElement);
					        uv: Value(SGroupElement); vv: Value(SGroupElement)} //no data for this
9. ProveDlog => {value: Value(SGroupElement)}
10. CaseObject => Nothing
11. SigmaPropIsProven => {input: Value(SSigmaProp)}
12. SigmaPropBytes => {input: Value(SSigmaProp)}
13. ConcreteCollectionBooleanConstant => {items.size: UShort; items: toArray >> Bits}
14. ConcreteCollection => {items.size: UShort; tpe.elemType: Type; items: Value*}
15. LogicalTransformer => {input: Value(Coll[SBoolean])}
16. TaggedVariable => {varId: Byte; tpe: Type}
17. GetVar => {varId: Byte; tpe.elemType: Type}
18. MapCollection => {input: Value(SCollection[?]); mapper: Value(SFunc)}
19. BooleanTransformer => {input: Value(SCollection[?]); condition: Value(SFunc)}
20. Fold => {input: SCollection[?]; zero: Value; foldOp: Value(SFunc)}
21. SimpleTransformer => {input: Value}
22. OptionGetOrElse => {input: Value(SOption[?]); default: Value}
23. DeserializeContext => {tpe: Type; id: Byte}
24. DeserializeRegister => {reg.number: Byte; tpe: Type; default: Option(Value)} //no data for this
25. ExtractRegisterAs => {input: Value(SBox); registerId.number: Byte; tpe.elemType: Type}
26. Filter => {id: Byte; input: Value(SCollection[?]); condition: Value(SBoolean)}
27. Slice => {input: Value(SCollection[?]); from: Value(SInt); until: Value(SInt)}
28. AtLeast => {bound: Value(SInt); input: Value(SCollection[SSigmaProp])}
29. ByIndex => {input: SCollection[?]; index: Value(SInt); default: Option}
30. Append => {input: Value(SCollection[?]); col2: Value(SCollection[?])}
31. NumericCast => {input: Value(SNumericType); tpe: Type}
32. [opCode != FunDefCode] | ValDef = {id: UInt; rhs: Value}
    [opCode == FunDefCode] | ValDef = {id: UInt; tpeArgs.length.toByteExact: Byte; tpeArgs: Type*; rhs: Value}
33. BlockValue => {items.length: UInt; items: Value*; result: Value}
34. ValUse = {valId: UInt}
35. FuncValue => {args.length: UInt; (args.idx, args.tpe): (UInt, Type)*; body: Value}
36. Apply => {func: Value; args: Values}
37. [opCode != OpCodes.MethodCallCode] | MethodCall = {method.objType.typeId: Byte; method.methodId: Byte; obj: Value}
    [opCode == OpCodes.MethodCallCode] | MethodCall = {method.objType.typeId: Byte; method.methodId: Byte; obj: Value; args: Values}
38. SigmaTransformer => {items.length: UInt; items: Value(SigmaPropValue)*}
39. BoolToSigmaProp => {value: Value(BoolValue)}
40. ModQ => {input: SBigInt}
41. ModQArithOp => {left: Value(SBigInt); right: Value(SBigInt)}
42. SubstConstants => {scriptBytes: Value(SByteArray); positions: Value(SIntArray); newValues: Value(SCollection[?])}
43. ErgoBox => {box: Body (ErgoBoxCandidate); transactionId.toBytes: Array [Bytes]; index: UShort}
44. ErgoBoxCandidate => {value: ULong; ergoTree: ErgoTree >> Array[Byte]; creationHeight: UInt;
                         additionalTokens.size: UByte;
			 additionalTokens: [digestsInTx.isDefined] | (UInt, ULong)*
			                   [_] | (Array [Byte], ULong)*;
			 nRegs: UByte; regs: Value*}
45. GroupElement => (toByteArray >> Array [Byte])
46. AvlTreeData => {startingDigest.length: UByte; startingDigest: Array[Byte]; keyLength: UInt;
                   valueLengthOpt: Option (UInt); maxNumOperations: Option (UInt); maxDeletes: Option (UInt)}
47. [with segregation] | ErgoTree => {header: [with segregation] ErgoTreeHeader; tree: Value >> toBytes >> Array[Byte]}
    [without segregation] | ErgoTree => {header: ErgoTreeHeader; root: Value}
48. [with segregation] | ErgoTreeHeader => {header: Byte; constants.length: UInt; constants: Constant*}
    [without segregation] | ErgoTreeHeader => {header: Byte}


