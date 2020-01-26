## Adding new contract

Prerequisites:
- Install Z3 SMT solver from https://github.com/Z3Prover/z3

### 1. Subclass `SigmaContract` in the `verified-contracts` project
### 2. Create a method for the contract
First parameter have to be `ctx: Context` and subsequent parameters may be contract parameters. Return value have to be `SigmaProp`. Make the first line of the contract code `import ctx._` to improve readability.
### 3. Write contract code in the method.
See [buy order](http://github.com/ScorexFoundation/sigmastate-interpreter/blob/346717a7d6af45c685b617ccea7babcf39d49939/contract-verification/src/main/scala/sigmastate/verification/contract/AssetsAtomicExchange.scala#L12-L32)

### 4. Create a subclass (object) of the class with contract code to make "instance" method to compile the code.
It'll a place to invoke the compiler (macros) and embed contract parameters. Create a method with parameters from the contract (without `Context` parameter) and invoke `ErgoContractCompiler.compile`. See [buy order example](http://github.com/ScorexFoundation/sigmastate-interpreter/blob/346717a7d6af45c685b617ccea7babcf39d49939/contract-verification/src/main/scala/sigmastate/verification/contract/AssetsAtomicExchange.scala#L116-L122)
Mark this method with `@ignore` annotation to hide it from Stainless. 

### 5. Call the "instance" method in another module/project.
Call returned `ErgoContract.scalaFunc` to run the contract with given `Context`. See [buy order tests](http://github.com/ScorexFoundation/sigmastate-interpreter/blob/346717a7d6af45c685b617ccea7babcf39d49939/sigma-dsl-compiler-macros-playground/src/test/scala/sigmastate/verification/test/AssetsAtomicExchangeCompilationTest.scala#L177-L220)