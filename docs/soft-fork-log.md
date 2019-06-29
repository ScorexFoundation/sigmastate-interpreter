
## A log of changes leading to soft-fork

This list should be updated every time something soft-forkable is added.

### Changes in v2.1

 - new type (SGlobal.typeCode = 106)
 - new method (SGlobal.groupGenerator.methodId = 1)
 - new method (SAvlTree.updateDigest.methodId = 15)
 - removed GroupElement.nonce (changed codes of getEncoded, exp, multiply, negate) 
 - change in Coll.filter serialization format (removed tagged variable id, changed condition type)   
 
### Changes in v2.2
 
#### Changes in ErgoConstants
 MaxTokens  4 -> 255
 MaxPropositionBytes  64K -> 4K
 SizeBoxBytesWithoutRefsMax 64K -> 4K
 MaxSigmaPropSizeInBytes  1K  (added because SigmaProp.isConstantSize == true)
 MaxLoopLevelInCostFunction 1  (added and checked)
 
#### ComplexityTable added
 
#### Changes in CostTable

MinimalCost = 10 (1)
interpreterInitCost = 10000 (added)
perGraphNodeCost = 200 (added)
val costFactor: Double = 2d (added)
constCost = 10 (1)
lambdaCost = 10 (1)
plusMinus = 10 (2)
comparisonCost = 10 (3)
lambdaInvoke = 30 (added)
concreteCollectionItemCost = 10 (added)  // since each item is a separate graph node 
logicCost = 10 (2) 
castOp = 10 (5)