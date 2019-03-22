(NB: outdated - current version went to Google Sheets)

# Native
1. Nothing => {}
2. Byte => APPEND
2. UInt => ULong
3. Boolean => APPEND
4. Array [Byte] => APPEND
5. Array [Boolean] => (toBytes >> Array [Byte])
5. UShort => UInt
6. Long => (ZigZagLong >> ULong)
7. Int => (ZigZagInt >> ULong)
8. ULong => (VLQEnc >> APPEND)
9. Short => APPEND
10. [None] | Option => {header: Byte (0)}
    [Some v] | Option => {header: Byte (1); v: Serializer}

# Data
1. SUnit => Nothing
2. SBoolean => Boolean
3. SByte => Byte
4. SShort => Short
5. SInt => Int
6. SLong => Long
7. SString => {length: UInt; bytes: encode >> Array [Byte]}
8. SBigInt => {length: UShort; bytes: toByteArray >> Array [Byte]}
9. SGroupElement => (Body (GroupElement))
10. SSigmaProp => (Value (SigmaBoolean))
11. SBox => (Body (ErgoBox))
12. SAvlTree => (Body (AvlTreeData))
13. [SBoolean] | SCollectionType => {length: UShort; bits: toBits >> Array [Boolean]}
    [SByte] | SCollectionType => {length: UShort; bytes: toBytes >> Array [Byte]}
    [_]     | SCollectionType => {length: UShort; elems: Data*}
14. STuple => (toArray >> Data*)

#Type

# Value
1. Values = {length: UInt; values: Value*}
2. [Non-constant] | Value = {opCode: Byte; body: Body}
   [Constant with store] | Value = {placeholder.opCode: Byte; constantPlaceholder: Body(ConstantPlaceholder)}
   [Constant without store] | Value = {const: Body(Constant)}

# Body
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