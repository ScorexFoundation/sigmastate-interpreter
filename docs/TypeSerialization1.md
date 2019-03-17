# Type (maybe not needed)
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
