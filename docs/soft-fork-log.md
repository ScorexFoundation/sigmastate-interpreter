
## A log of changes leading to soft-fork

This list should be updated every time something soft-forkable is added.

### Changes in v2.1

 - new type (SGlobal.typeCode = 106)
 - new method (SGlobal.groupGenerator.methodId = 1)
 - new method (SAvlTree.updateDigest.methodId = 15)
 - removed GroupElement.nonce (changed codes of getEncoded, exp, multiply, negate) 
 - change in Coll.filter serialization format (removed tagged variable id, changed condition type)   
 
### Changes in v2.2
 
 MaxTokens  4 -> 255
 MaxPropositionBytes  64K -> 4K
 SizeBoxBytesWithoutRefsMax 64K -> 4K
 MaxSigmaPropSizeInBytes  1K  (added because SigmaProp.isConstantSize == true)
 MaxLoopLevelInCostFunction 1  (added and checked)
 
 ComplexityTable added