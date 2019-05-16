
## A log of changes leading to soft-fork

This list should be updated every time something soft-forkable is added.

### Changes since 2.0

 - new type (SGlobal.typeCode = 106)
 - new method (SGlobal.groupGenerator.methodId = 1)
 - new method (SAvlTree.updateDigest.methodId = 15)
 - removed GroupElement.nonce (changed codes of getEncoded, exp, multiply, negate) 
 - change in Coll.filter serialization format (removed tagged variable id, changed condition type)   