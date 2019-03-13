# Native types serialization
1. Nothing =>
2. Byte => *append*
2. UInt => ULong
3. Boolean => *append*
4. Array [Byte] => *append*
5. Array [Boolean] => (toBytes >>= Array [Byte]) // *toBytes xs = if (xs.isEmpty) **Nothing** else
    val bitSet = new BitSet(xs.length)
    xs.zipWithIndex.foreach { case (bool, i) => bitSet.set(i, bool)}
    val bytes = Arrays.copyOf(bitSet.toByteArray, (xs.length + 7) / 8)*
5. UShort => UInt
6. Long => ZigZagLong >>= ULong
7. Int => ZigZagInt >>= ULong
8. ULong => VLQ encoding >>= *append*
9. Short => *append*
10. Option X => (None => Byte (0)) | (Some v => Byte (1) >> v)

# S-types serialization (data)
1. SUnit => Nothing
2. SBoolean => Boolean
3. SByte => Byte
4. SShort => Short
5. SInt => Int
6. SLong => Long
7. SString => (*length* >>= UInt) >> (*encode* >>= Array [Byte]) //*encode = getBytes(StandardCharsets.UTF_8)*
8. SBigInt => (*array length* >>= UShort) >> (*BigInteger.toByteArray* >>= Array [Byte])
9. SGroupElement => (*EcPointType.toByteArray* >>= Array [Byte]) //*toByteArray = if (point.isInfinity) {
      identityPointEncoding
    } else {
      val normed = point.normalize()
      val ySign = normed.getAffineYCoord.testBitZero()
      val X = normed.getXCoord.getEncoded
      val PO = new Array[Byte](X.length + 1)
      PO(0) = (if (ySign) 0x03 else 0x02).toByte
      System.arraycopy(X, 0, PO, 1, X.length)
      PO
    }*
10. SSigmaProp => (SigmaBoolean => ???)
11. SBox => (*this* >>= ErgoBoxCandidate body) >>
            (*txIdBytes* >>= Array [Bytes]) >>
            (*index* >> UShort) //*val txIdBytes = obj.transactionId.toBytes
12. ErgoBoxCandidate => serializeBodyWithIndexedDigests ??
13. SAvlTree => (*length* *startingDigest* >>= UByte) >>
        (*startingDigest* >>= Array[Byte]) >>
        (*keyLength* >>= UInt) >>
        (*valueLengthOpt* >>= Option UInt) >>
        (*maxNumOperations* >>= Option UInt) >>
        (*maxDeletes* >>= Option UInt)
13. SCollectionType => (*length* >>= UShort) >> ((*of* SBoolean => Array [Boolean]) | (*of* SByte => Array [Byte]) | (_ => *map* serialize_data))
14. STuple => (toArray >>= *map* serialize_data) //*toArray=v.asInstanceOf[t.WrappedType] (SAny?)*

# Type (self)
1. (p: SEmbeddable) => (*p.typeCode* >>= Byte) //?? what are embeddable types
2. SString => (102 >>= Byte)
3. SAny => (97 >>= Byte)
4. SUnit => (98 >>= Byte)
5. SBox => (99 >>= Byte)
6. SAvlTree => (100 >>= Byte)
7. SCollectionType [a] =>
    7.1. a of (p: Embeddable) => (*CollectionTypeCode* + *p.typeCode*) >>= Byte
    7.2. a of (cn: SCollectionType [b]) => 
        7.2.1. b of (r:Embeddable) => (*NestedCollectionTypeCode* + r.typeCode) >>= Byte
        7.2.2. b of _ => (*CollectionTypeCode* >>= Byte) >> (cn >>= *serialize_type*)
    7.3. a of t => (*CollectionTypeCode* >>= Byte) >> (t >>= *serialize_type*)
8. SOption [a] => 
    8.1. a of (p: Embeddable) => (*OptionTypeCode* + *p.typeCode*) >>= Byte
    8.2. a of (c: SCollectionType [b]) => 
        8.2.1. b of (r:Embeddable) => (*OptionCollectionTypeCode* + r.typeCode) >>= Byte
        8.2.2. b of _ => (*OptionTypeCode* >>= Byte) >> (c >>= *serialize_type*)
    8.3. a of t => (*OptionTypeCode* >>= Byte) >> (t >>= *serialize_type*)
9. STuple => 
    9.1. (p: Embeddable, p) => (*PairSymmetricTypeCode* + *p.typeCode*) >>= Byte
    9.2. (p: Embeddable, r) => ((*Pair1TypeCode* + *p.typeCode*) >>= Byte) >> (r >>= *serialize_type*)
    9.3. (p, r: Embeddable) => ((*Pair2TypeCode* + *r.typeCode*) >>= Byte) >> (p >>= *serialize_type*)
    9.4. (p, r) => (*Pair1TypeCode* >>= Byte) >> (p >>= *serialize_type*) >> (r >>= *serialize_type*)
    9.5. (p,r,q) => (*TripleTypeCode* >>= Byte) >> (p >>= *serialize_type*) >> (r >>= *serialize_type*) >> (q >>= *serialize_type*)
    9.6. (p,r,q,s) => (*QuadrupleTypeCode* >>= Byte) >> (p >>= *serialize_type*) >> (r >>= *serialize_type*) >> (q >>= *serialize_type*) >> (s >>= *serialize_type*)
    9.7. (,..,) => (*TupleTypeCode* >>= Byte) >> (*length* >>= UByte) >> (*map* *serialize_type*)
10. STypeIdent => (103 >>= Byte) >> (*name* >>= SString as data)

# Other constructions
1. **Constant** => (*Constant Type* >>= **Type**) >> **Data**
2. **OpCode** => Byte
3. **Value** of **Non-constant** => (*OpCode* >>= **OpCode**) >> (*Body* >>= ?)
4. **DeserializeRegisterSerializer** => (*Register number* >>= Byte) >> (*Object Type* >>= **Type**) >> (*Object default* >>= Option)
5. **SigmaPropIsProvenSerializer** => (*Object.input* >>= **Value** of **Non-constant**)
6. 






