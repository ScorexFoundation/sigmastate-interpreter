# Ergo serialization format specification

## Goal and structure
The goal of this paper is to summarize and formalize the serialization procedure of the ``ErgoTree`` and depended objects. During the serialization objects of different types appear. All the type kinds can be considered separately and therefore the document contains correspondent sections  dedicated to the particular kinds. Further in each section for each type (or serialization case) the particular format is given. 

## Format
We use the following format to describe a particular serialization case:
 ``S => {Field(1), Field(2),...}``, where ``S`` is currently being described type or serialization case and the content inside the braces are fields placed one by one. 

Each ``Field(i)`` has its own format ``Field = name: tr(1) >> tr(2) >> ... >> S``, where ``name`` is the both the field name and some property of the serialized object (some exceptions here may occur, we will note it). `tr(i)` is the transformer to be used on the ``name`` property before piping further. ``S`` is already defined serializer or serializers family (if cannot be determined). Note that ``S`` can be recursively defined. 

When we need to store a particular constant we also use ``>>`` notation like ``const >> S`` (e.g. ``0 >> Byte``) meaning that we serialize exactly this value, but not something associated with the name ``const``. 

It can be a case when serializing compound object that it contains structures of undetermined length. So we need the specification format which support this case. When serializing structures like arrays or tuples we meet the iteration over their elements, in this case we use pattern ``objects: S*`` meaning that each ``object`` from a collection is serialized by the ``S`` serializer. 

Each serializer is included in some family as mentioned before. When we need to use the particular serializer from a family    
we use the notation ``F(S)`` with ``F`` standing for the family name, and ``S`` for particular serializer inside the family.

Serializer can potentially have several cases when depending on the object property or intentionally being serialized in different way. We denote this as ``[condition] | S => ...`` where ``condition`` presents the case when ``S`` is used as written after ``=>`` sign.  ``[_]`` condition pattern stands for all other cases and is used as the last one.

When serializer has exceptional behavior and requires some constrains to be hold we denote it in ``[assert]`` block below the main definition.

In some cases we provide the particular Scala code for transformers or conditions below the serializer definition. Also we provide the type of serialized object before the definition.

Concluding, the final format is:
```scala
obj: Type
[condition] | S => {name1: tr(1,1) >> tr(1,2) >> ... >> [condition] S(1); ...} 
[assertions...]
```

In some cases we omit the ``name`` part, then the transformers are applied to the whole serialized object. We can also omit the braces in the case when only one (unnamed) field is being serialized. 

Further each section name also corresponds to serializers family. 

## Basic
The serialization is performed by storing the correspondent bytes in ``ByteWriter`` object inherited from ``ByteArrayBuilder`` java class which has besides others the following basic operations:
```scala
append (x: Byte)
append (x: Short)
append (x: Array [Byte])
append (x: Boolean)
```
which append the correspondent value to the internal storage in polymorphic way. All of these operations we denote as basic ``APPEND`` serializer. The appending procedure is developed inside the standard library and is beyond the scope of this document. 

We also use the ``NOTHING`` serializer which does nothing for notation convenience.

## Native
Native types are types given by the Scala standard library and are serialized as follows:
1.  Byte
```scala
x: Byte
Byte => APPEND
```

2. UByte
```scala
x: Int 
UByte => Byte
[assert] (x >= 0 && x <= 0xFF)
```

3. UInt
```scala
x: Long
UInt => ULong
[assert] (x >= 0 && x <= 0xFFFFFFFFL)
```

4. Boolean
```scala
x: Boolean
Boolean => APPEND
```

5. Bytes
```scala
x: Array [Byte]
Bytes => APPEND
```

6.  Bits
```scala
xs: Array [Boolean] 
Bits => (toBytes >> Bytes)
```
where ``toBytes`` is defined as 
```scala
	{ 
	  if (xs.isEmpty) Empty //empty array
	  val bitSet = new BitSet(xs.length)  
	  xs.zipWithIndex.foreach { case (bool, i) => bitSet.set(i, bool)}  
	  val bytes = Arrays.copyOf(bitSet.toByteArray, (xs.length + 7) / 8)  
	  bytes
	}
```

7. UShort
```scala
x: Int
UShort => UInt
[assert] (x >= 0 && x <= 0xFFFF)
```

8. Long
```scala
x: Long
Long => (ZigZagLong >> ULong)
```
where ``ZigZagLong (x: Long) : Long`` is defined following the [code](http://github.com/google/protobuf/blob/a7252bf42df8f0841cf3a0c85fdbf1a5172adecb/java/core/src/main/java/com/google/protobuf/CodedOutputStream.java#L949). 

9. Int
```scala
x: Int
Int => (ZigZagInt >> ULong)
```
where ``ZigZagInt (x: Int) : Int`` is defined following the [code](http://github.com/google/protobuf/blob/a7252bf42df8f0841cf3a0c85fdbf1a5172adecb/java/core/src/main/java/com/google/protobuf/CodedOutputStream.java#L934). 

10. ULong
```scala
x: Long
ULong => (encodeVLQ >> Bytes)
```
where ``encodeVLQ (x: Long): Array [Byte]`` is defined following the [code](http://github.com/google/protobuf/blob/a7252bf42df8f0841cf3a0c85fdbf1a5172adecb/java/core/src/main/java/com/google/protobuf/CodedOutputStream.java#L1387). 

11. Short
```scala
x: Short
Short => APPEND
```

12. Option

There are two cases for serializing ``Option`` typed value - for ``None`` and ``Some`` pattern matching results.
```scala
x: Option X
12.1 [None] | Option => {header: 0 >> Byte}
12.2 [Some v] | Option => {header: 1 >> Byte; 
                           v: Serializer}
```
with arbitrary externally passed ``Serializer`` for ``v``.

## Data
The current section is dedicated for S-typed objects data serialization. Some S-types have implicit coercion to correspondent native types performed by ``v.asInstanceOf[T]`` call with ``v`` of some S-type and ``T`` is coercible (nested) native type.   The implicit conversion transformation is omitted in notation.
1. SUnit
```scala
x: SUnit
SUnit => NOTHING
```
2. SBoolean
```scala
x: SBoolean
SBoolean => Native (Boolean)
```

3. SByte
```scala
x: SByte
SByte => Native (Byte)
```

4. SShort
```scala
x: SShort
SShort => Native (Short)
```

5. SInt
```scala
x: SInt
SInt => Native (Int)
```

6. SLong
```scala
x: SLong
SLong => Native (Long)
```

7.  SString
```scala
x: SString
SString => {length: Native (UInt);
            bytes: encodeUTF8 >> Native (Bytes)}
```
where ``length`` stands for the ``Array.length()`` return value for the array produced by ``encodeUTF8`` transformer which is defined as ``String.getBytes(StandardCharsets.UTF_8)`` for serialized string.

8. SBigInt
```scala
x: SBigInt
SBigInt => {length: Native (UShort);
            bytes: toByteArray >> Bytes}
```
where ``length`` stands for the ``Array.length()`` return value for the array produced by ``toByteArray`` transformer which is defined as ``BigInteger.toByteArray()`` for serialized object.

9. SGroupElement
```scala
x: SGroupElement
SGroupElement => Body (GroupElement)
```
Here we refer to the ``Body`` serializers family defined below and the used coercion is ``SGroupElement -> EcPointType``.

10. SSigmaProp
```scala
x: SSigmaProp
SSigmaProp => Value
```
Here we refer to the ``Value`` serializers family defined below and the used coercion is ``SSigmaProp -> SigmaBoolean``.

11. SBox
```scala
x: SBox
SBox => Body (ErgoBox)
```
Here we refer to the ``Body`` serializers family defined below and the used coercion is ``SBox -> ErgoBox``.

12. SAvlTree
```scala
x: SAvlTree
SAvlTree => Body (AvlTreeData)
```
Here we refer to the ``Body`` serializers family defined below and the used coercion is ``SAvlTree -> AvlTreeData``.

13. SCollectionType
```scala
x: SCollectionType [X]
13.1 [X == SBoolean] | SCollectionType => {length: Native (UShort);
                                           bits: Native (Bits)}
```
Here ``length`` is the collection length defined by ``Array.length()`` for coerced object and the explicit coercion ``SCollectionType[SBoolean] -> Array [Boolean]`` to get ``bits``.

```scala
13.2 [X == SByte] | SCollectionType => {length: Native (UShort);
                                        bytes: Native (Bytes)}
```
Here ``length`` is the collection length defined by ``Array.length()`` for coerced object and the explicit coercion ``SCollectionType[SByte] -> Array [Byte]`` to get ``bytes``.

```scala
13.3 [_] | SCollectionType => {length: Native (UShort);
                               elems: Data*}
```
Here ``length`` is the collection length defined by ``Array.length()`` for the object produced by the general coercion ``SCollectionType[X] -> Array[X#WrappedType]``. The serialization for  ``elems`` is produced by mapping each element with correspondent ``Data`` family serializer.

14.  STuple
```scala
x: STuple
STuple => Data*
[assert] (arr.length == items.length)
[assert] (arr.length <= 0xFFFF)
```
with coercion  ``STuple -> Array[Any]`` , where ``arr`` is the coerced array and ``items`` is the array of the serialized tuple types.

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

1.  Value
```scala
x: Value[T]; T <: SType
1.1 [Non-constant] | Value => {opCode: Native (Byte);
							   body: Body}
1.2 [Constant with store] | Value => {ConstantPlaceholderIndexCode >> Native (Byte);
									  constantPlaceholder: Body (ConstantPlaceholder)}
1.3 [Constant without store] | Value => Body (Constant)
```
2. Values
```scala
xs: Seq[Value[T]]; T <: SType
Values = {length: UInt;
		  values: Value*}
```
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

The constant ``LastConstantCode`` is defined in turn as ``LastDataType + 1``,  with ``LastDataType = 111``.

## Body
The ``Body`` for ``Value`` serializers do the main job to store the body of the correspondent operations in the tree and are given below. 

The field names correspond to the names of correspondent serialized object properties. The object type is also given before the serializer definition in notation of ``obj: Type``.

### Body for ``Value`` serializers
1.  Constant
```scala
obj: Constant[SType]
Constant => {tpe: Type;
		     value: Data}
```
2.  ConstantPlaceholder
```scala
obj: ConstantPlaceholder[SType]
ConstantPlaceholder => {id: Native (UInt)}
```

3.  Tuple
```scala
obj: Tuple
Tuple => {length: Native (UByte); 
		  items: Value*}
```
where length is the tuple length and each item is serialized as separate ``Value`` object.

4.  SelectField
```scala
obj: SelectField
SelectField => {input: Value; 
                fieldIndex: Native (Byte)}
```

5.  Relation2

There are two cases when serializing ``Relation2`` object, first - for serializing ``Boolean`` constants and otherwise.
```scala
obj: Relation[S1, S2]; S1 <: SType; S2 <: SType
5.1 [BooleanConstants] | Relation2 => {ConcreteCollectionBooleanConstantCode >> Native (Byte); 
                                       LeftRightBits: toBits >> Native (Bits)}
5.2 [_] | Relation2 => {left: Value; 
                    right: Value}
 ```
where 
```scala
toBits = Array [Boolean](left.asInstanceOf[Boolean], right.asInstanceOf[Boolean])
ConcreteCollectionBooleanConstantCode = LastConstantCode + 21
```  
6.  Quadruple
```scala
obj: Quadruple[S1, S2, S3, S4]
Quadruple => {first: Value; 
              second: Value; 
              third: Value}
```

7.  TwoArguments
```scala
obj: TwoArgumentsOperation[LIV, RIV, LIV]
TwoArguments => {left: Value; 
                 right: Value}
```

8.  ProveDHTuple

There are two cases when serializing ``ProveDHTuple`` object, first - for serializing ``SGroupElement`` constants and otherwise.
```scala
obj: ProveDHTuple
8.1 [SGroupElementConstants] | ProveDHTuple => {constCodePrefix >> Native (Byte); 
												SGroupElementType >> Type;
	                                            gv_data: Data (SGroupElement); 
	                                            hv_data: Data (SGroupElement);
	                                            uv_data: Data (SGroupElement); 
	                                            vv_data: Data (SGroupElement)}
8.2 [_] | ProveDHTuple => {gv: Value; 
						   hv: Value;
						   uv: Value; 
						   vv: Value}
```
where ``constCodePrefix = 0; SGroupElementType = SGroupElement``. 

9.  ProveDlog
```scala
obj: ProveDlog
ProveDlog => {value: Value}
 ```
 
10.  CaseObject
```scala
obj: V; V <: Value[SType]
CaseObject => NOTHING
```

11.  SigmaPropIsProven
```scala
obj: SigmaPropIsProven
SigmaPropIsProven => {input: Value}
```

12.  SigmaPropBytes
```scala
obj: SigmaPropBytes
SigmaPropBytes => {input: Value}
```

13. ConcreteCollectionBooleanConstant
 ```scala
obj: ConcreteCollection[SBoolean.type]
ConcreteCollectionBooleanConstant => {items.size: Native (UShort); 
                                      items: toArray >> Native (Bits)}
```
where 
```scala
toArray = obj.items.map {  
  case v: Constant[SBoolean.type] => v.value  
  case v => error()  
}.toArray
```
The serializer leads to  ``error`` when gets not ``Boolean`` constant inside the list.

14.  ConcreteCollection
```scala
obj: ConcreteCollection[_ <: SType]
ConcreteCollection => {items.size: Native (UShort); 
                       tpe.elemType: Type; 
                       items: Value*}
```
``items`` are serialized one by one by ``foreach`` iterator.

15. LogicalTransformer
```scala 
obj: Transformer[I, O]; I <: SCollection[SBoolean.type]; O <: SBoolean.type
LogicalTransformer => {input: Value}
```

16. TaggedVariable
```scala
obj: TaggedVariable[_ <: SType]
TaggedVariable => {varId: Native (Byte); 
                   tpe: Type}
```

17.  GetVar
```scala
obj: GetVar[_ <: SType]
GetVar => {varId: Native (Byte); 
           tpe.elemType: Type}
```

18.  MapCollection
```scala
obj: MapCollection[SType, SType]
MapCollection => {input: Value; 
                  mapper: Value}
```

19. BooleanTransformer
```scala
obj: BooleanTransformer[T]; T <: SType
BooleanTransformer => {input: Value; 
                       condition: Value}
```

20. Fold
```scala
obj: Fold[SType, SType]
Fold => {input: Value; 
         zero: Value; 
         foldOp: Value}
```

21. SimpleTransformer
```scala
obj: Transformer[I, O]; I <: SType, O <: SType
SimpleTransformer => {input: Value}
```

22. OptionGetOrElse
```scala
obj: OptionGetOrElse[_ <: SType]
OptionGetOrElse => {input: Value; 
					default: Value}
```

23. DeserializeContext
```scala
obj: DeserializeContext[SType]
DeserializeContext => {tpe: Type; 
                       id: Native (Byte)}
```

24. DeserializeRegister
```scala
obj: DeserializeRegister[SType]
DeserializeRegister => {reg.number: Native (Byte); 
                        tpe: Type; 
                        default: Native (Option (Value))} 
```
where ``default`` is serialized as ``Native (Option)`` with ``Value`` as passed serializer for ``Some`` case (see ``Option`` serializer definition at the ``Native `` section.

25. ExtractRegisterAs
```scala
obj: ExtractRegisterAs[SType]
ExtractRegisterAs => {input: Value; 
                      registerId.number: Native (Byte); 
                      tpe.elemType: Type}
```

26. Filter
```scala
obj: Filter[SType]
Filter => {id: Natve (Byte); 
           input: Value; 
           condition: Value}
```

27. Slice
```scala
obj: Slice[SType]
Slice => {input: Value; 
		  from: Value; 
		  until: Value}
```

28. AtLeast
```scala
obj: AtLeast
AtLeast => {bound: Value; 
			input: Value}
```

29. ByIndex
```scala
obj: ByIndex[SType]
ByIndex => {input: Value; 
            index: Value;
            default: Native (Option (Value))}
```
where ``default`` is serialized as ``Native (Option)`` with ``Value`` as passed serializer for ``Some`` case (see ``Option`` serializer definition at the ``Native `` section.

30. Append
```scala
obj: Append[SType]
Append => {input: Value; 
		   col2: Value}
```

31. NumericCast
```scala
obj: Transformer[SNumericType, SNumericType]
NumericCast => {input: Value; 
				tpe: Type}
```

32. ValDef

There are two cases when serializing ``ValUse`` object - for ``FunDefCode`` ``opCode`` and otherwise. In the first case the additional information is to be stored.
```scala
obj: ValDef
32.1 [opCode == FunDefCode] | ValDef => {id: Native (UInt); 
										 tpeArgs.length.toByteExact: Native (Byte); 
										 tpeArgs: Type*; 
										 rhs: Value}
[assert] (!obj.isValDef), isValDef = tpeArgs.isEmpty
[assert] (obj.tpeArgs.nonEmpty), nonEmpty = !isEmpty
TODO: seems that those are equal assertions?
32.2 [_] | ValDef => {id: Native (UInt); 
				 rhs: Value}
```

33. BlockValue
```scala
obj: BlockValue
BlockValue => {items.length: Native (UInt); 
               items: Value*; 
               result: Value}
```
``items`` are serialized using ``foreach`` iterator.

34. ValUse
```scala
obj: ValUse[SType]
ValUse = {valId: Native (UInt)}
```

35. FuncValue
```scala
obj: FuncValue
FuncValue => {args.length: Native (UInt); 
              args@(idx, tpe): (Native (UInt), Type)*; 
              body: Value}
```
where we use ``lhs@rhs`` to synonymize ``lhs`` and ``rhs``, ``args``  are serialized using ``foreach`` iterator.

36. Apply
```scala
obj: Apply
Apply => {func: Value; 
          args: Values}
```

37. MethodCall

There are two cases when serializing ``MethodCall`` object - for ``MethodCallCode`` ``opCode`` and otherwise. In the first case the additional information is stored.
```scala
obj: MethodCall
37.1 [opCode == MethodCallCode] | MethodCall => {method.objType.typeId: Native (Byte); 
                                            method.methodId: Native (Byte); 
                                            obj: Value; 
                                            args: Values}
[assert] (args.nonEmpty)
37.2 [_] | MethodCall => {method.objType.typeId: Native (Byte); 
					 method.methodId: Native (Byte); 
					 obj: Value}
```

38. SigmaTransformer
```scala
obj: SigmaTransformer[I, O]; I <: SigmaPropValue, O <: SigmaPropValue; 
SigmaPropValue = Value[SSigmaProp.type]
SigmaTransformer => {items.length: Native (UInt); 
					 items: Value*}
```
``items``  are serialized using ``foreach`` iterator.

39. BoolToSigmaProp
```scala
obj: BoolToSigmaProp
BoolToSigmaProp => {value: Value}
```

40. ModQ
```scala
obj: ModQ 
ModQ => {input: Value}
```

41. ModQArithOp
```scala
obj: ModQArithOp
ModQArithOp => {left: Value; 
				right: Value}
```
42. SubstConstants
```scala 
obj: SubstConstants[SType]
SubstConstants => {scriptBytes: Value; 
				   positions: Value; 
				   newValues: Value}
```
43. Relation3 (unused)
```scala
obj: Relation3[S1, S2, S3]; S1 <: SType; S2 <: SType; S3 <: SType
Relation3 => {left: Value;  
			  right: Value;  
			  third: Value}
```

### Other ``Body`` serializers

There are also a number of body serializers not using to store the tree nodes but other objects. Here they are.

1. ErgoBox
```scala
obj: ErgoBox
ErgoBox => {obj: Body (ErgoBoxCandidate); 
            transactionId.toBytes: Native (Bytes); 
            index: Native (UShort)}
[assert] (transactionId.toBytes.length == ErgoLikeTransaction.TransactionIdBytesSize)
```
where ``ErgoLikeTransaction.TransactionIdBytesSize = 32``.

2. ErgoBoxCandidate
There are two cases to serialize ``serializeBodyWithIndexedDigests``. The default ``ErgoBoxCandidate`` serialization passes ``None`` as ``digestsInTx``, so we got to the second case. The first is given for convenience.
```scala
obj: ErgoBoxCandidate
ErgoBoxCandidate => serializeBodyWithIndexedDigests (digestsInTx = None)
2.1 [digestsInTx.isDefined] | serializeBodyWithIndexedDigests => {value: Native (ULong); 
									ergoTree: [without segregation] ErgoTree >> Native (Bytes); 
									creationHeight: Native (UInt);
									additionalTokens.size: Native (UByte);
									additionalTokens@(id, amount): 
									   (digestsInTx.get.indexOf(id) >> Native (UInt), Native (ULong))*;
									nRegs: Native (UByte); 
									regs: Value*}
[assert] (forall (id) from additionalTokens, digestsInTx.get.indexOf(id) != -1)
2.2 [_] | serializeBodyWithIndexedDigests => {value: Native (ULong); 
									ergoTree: [without segregation] ErgoTree >> Native (Bytes); 
									creationHeight: Native (UInt);
									additionalTokens.size: Native (UByte);
									additionalTokens@(id, amount): (Native (Bytes), Native (ULong))*;
									nRegs: Native (UByte); 
									regs: Value*}
[assert, both cases] (nRegs + ErgoBox.startingNonMandatoryIndex < 255)
[assert, both cases] (forall (regId) from startReg to endReg, 
                      ErgoBox.findRegisterByIndex(regId.toByte).get != None)
```
where ``isDefined = !isEmpty`` for ``Option`` class,
``nRegs = obj.additionalRegisters.keys.size``, 
``ErgoBox.startingNonMandatoryIndex = nonMandatoryRegisters.head.number``,
``nonMandatoryRegisters = Vector(R4, R5, R6, R7, R8, R9)``,
``startReg = ErgoBox.startingNonMandatoryIndex ``
``endReg = ErgoBox.startingNonMandatoryIndex + nRegs - 1``.

3. GroupElement (also EcPointType)
```scala
point: EcPointType
GroupElement => (toByteArray >> Native (Bytes))
```
where
```scala
val groupSize: Int = 256 / 8
encodingSize = 1 + CryptoConstants.groupSize
identityPointEncoding = Array.fill(encodingSize)(0: Byte)
toByteArray = if (point.isInfinity) {  
  identityPointEncoding  
} else {  
  val normed = point.normalize()  
  val ySign = normed.getAffineYCoord.testBitZero()  
  val X = normed.getXCoord.getEncoded  
  val PO = new Array [Byte](X.length + 1)  
  PO(0) = (if (ySign) 0x03 else 0x02).toByte  
  System.arraycopy(X, 0, PO, 1, X.length)  
  PO  
}
```

4. AvlTreeData
```scala
obj: AvlTreeData
AvlTreeData => {startingDigest.length: Native (UByte); 
				startingDigest: Native (Bytes); 
				keyLength: Native (UInt);
				valueLengthOpt: Native (Option (Native (UInt))); 
				maxNumOperations: Native (Option (Native (UInt))); 
				maxDeletes: Native (Option (Native (UInt)))}
```

5. ErgoTree
There are two different procedures to serialize ``ErgoTree`` - with constants segregation and without. In the case with constants segregation to serialize head the ``ErgoTree(ErgoTree.ConstantSegregationHeader, extractedConstants, null)`` object is passed with ``null`` tree root and ``extractedConstants = constantStore.getAll`` extracted from ``constantStore`` while serializing tree itself to the separate ``ByteWriter``. The ``tree`` is stored then as byte array extracted from the same separate ``ByteWriter``.
```scala
tree: Value[SType]
5.1 [with segregation] | ErgoTree => {header: [with segregation] ErgoTreeHeader; 
	                                  tree: Value >> toBytes >> Native (Bytes)}

obj: ErgoTree
5.2 [without segregation, default] | ErgoTree => {header: ErgoTreeHeader; 
											      root: Value}
```
6.  ErgoTreeHeader
Unlike the ``ErgoTree`` the header has one serialize procedure and the segregation flag is calculated inside it as 
``[with segregation] = ergoTree.isConstantSegregation`` where 
``isConstantSegregation = ErgoTree.isConstantSegregation (header)`` and 
``isConstantSegregation (header: Byte) = (header & ErgoTree.ConstantSegregationFlag) != 0``.  So the ``[with segregation]`` procedure for ``ErgoTree`` implies this by passing ``ErgoTree.ConstantSegregationHeader`` directly to header serialization procedure through its ``ErgoTree`` typed argument as shown in the ``ErgoTree`` section.
```scala
obj: ErgoTree
6.1 [with segregation] | ErgoTreeHeader => {header: Native (Byte); 
											constants.length: Native (UInt); 
											constants: Constant*}
6.2 [without segregation] | ErgoTreeHeader => {header: Native (Byte)}
```

7.  Operation
There are a number of cases depending on the operation kind which is got by matching the ``obj`` with patterns.
 ```scala 
7.1 obj: Operation
7.1.1 [Lookup (key)] | Operation => (tp=1, key) >> OperationKey 
7.1.2 [Remove (key)] | Operation => (tp=2, key) >> OperationKey
7.1.3 [RemoveIfExists (key)] | Operation => (tp=3, key) >> OperationKey
7.1.4 [Insert (key, value)] | Operation => (tp=4, key, value) >> OperationKeyValue
7.1.5 [Update (key, value)] | Operation => (tp=5, key, value)  >> OperationKeyValue
7.1.6 [InsertOrUpdate (key, value)] | Operation =>  (tp=6, key, value)  >> OperationKeyValue
7.1.7 [_] | Operation => 0 >> Native (Byte)
```
Please see the definitions for helper serializers ``OperationKey`` and ``OperationKeyValue`` below.
```scala
7.2 tp: Byte, key: Array [Byte]
OperationKey => {tp: Native (Byte);  
				 key: Native (Bytes)}
```
```scala
7.3 tp: Byte, key: Array [Byte], value: Array [Byte]
7.3.1 [valueLengthOpt.isEmpty] | OperationKeyValue => {(tp, key) >> OperationKey;  
													   value.length.toShort: Native (Short);  
												       value: Native (Bytes)}
7.3.2 [_] | OperationKeyValue => {(tp, key) >> OperationKey;  
							      value: Native (Bytes)}
```
where ``tp, key`` arguments are passed from the ``Operation`` serializer.

8.  ErgoLikeTransaction
```scala
tx: ErgoLikeTransaction
ErgoLikeTransaction => FlattenedTransaction(tx) >> Body (FlattenedTransaction)
```
where
```scala
def apply(tx: ErgoLikeTransaction): FlattenedTransaction =  
  FlattenedTransaction(tx.inputs.toArray, tx.outputCandidates.toArray)
```
9. FlattenedTransaction
```scala
ftx: FlattenedTransaction 
FlattenedTransaction => {inputs.length: Native (UShort);  
						 inputs: Body (Input)*;  
						 digests.length: Native (UInt);  
						 digests: Native (Bytes)*;  
						 outputCandidates.length: Native (UShort);  
						 outputCandidates: [digestsInTx.isDefined] | Body (ErgoBoxCandidate)*}
```
Please see ``Body`` serializer for ``ErgoBoxCandidate`` for ``[digestsInTx.isDefined]`` case. When performing  ``FlattenedTransaction`` serialization the ``Some (digests)`` param is passed, so the case is implied.

10. ContextExtension
```scala
obj: ContextExtension
ContextExtension => {values.size: Native (UByte);  
					 values@(id, v): (Native (Byte), v: Value)*}
```
11. ProverResult
```scala
obj: ProverResult
ProverResult => {proof.length: Native (UShort);  
				 proof: Native (Bytes);  
				 extension: Body (ContextExtension)}
```
12. Input
```scala
obj: Input
Input => {boxId: Native (Bytes);  
		  spendingProof: Body (ProverResult)}
```
13. UnsignedInput
```scala
obj: UnsignedInput
UnsignedInput => {boxId: Native (Bytes)}
```


