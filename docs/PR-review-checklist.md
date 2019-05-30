## What should be checked during PR review

### For each $TypeName.$methodName there should be

1. test case in SigmaDslTests (checks SigmaDsl <-> ErgoScript equality)
2. test case in CostingSpecification
3. costing rule method in ${TypeName}Coster
4. for each SMethod registration
     - .withInfo($description, $argsInfo)
     - .withIRInfo($irBuilder, $opDescriptor)

### For each PredefinedFunc registration there should be
     - PredefFuncInfo($irBuilder, $opDescriptor)
