[![CI](https://github.com/ScorexFoundation/sigmastate-interpreter/actions/workflows/ci.yml/badge.svg)](https://github.com/ScorexFoundation/sigmastate-interpreter/actions/workflows/ci.yml)

# ErgoScript compiler and ErgoTree interpreter

This repository contains implementations of ErgoScript compiler and ErgoTree
Interpreter for a family of Sigma-protocol based authentication languages (or simply
Sigma language).

This JS package is cross-compiled from [Scala
implementation](https://github.com/ScorexFoundation/sigmastate-interpreter) using Scala.js
compiler.

The modules published here can be used directly from JavaScript.

# Getting Started

Run following command to add Sigma.JS as a project dependency:

```bash
npm install sigmastate-js
```

# Package organization

All classes of this package are separated into several modules (which can also be thought
as layers). Each module contains a subset of all the class exported to JavaScript. You can
decide which modules to import in your application depending on which classes from this
package you want to use. 
Each subsequent module contains all the classes from the previous modules and some new
classes thus forming a layering of modules.

See TypeScript [definitions](sigmastate-js.d.ts) for the list of all exported classes,
methods and documentation.

NOTE, you only need to import only one of the modules, the one which contains all the
classes you need. This will allow optimizing the size of the final bundle.

The modules are:
- [sigma-core module](https://github.com/ScorexFoundation/sigmastate-interpreter/tree/b26930c5e7aa58b6d76dda96ab56db59825f8638/core) - contains core classes of the library
  - [Type](https://github.com/ScorexFoundation/sigmastate-interpreter/blob/b26930c5e7aa58b6d76dda96ab56db59825f8638/core/js/src/main/scala/sigma/js/Type.scala)
  - [Value](https://github.com/ScorexFoundation/sigmastate-interpreter/blob/2d767ae75ab233deefeba25e42ca22ae22be8952/core/js/src/main/scala/sigma/js/Value.scala)
  - [GroupElement](https://github.com/ScorexFoundation/sigmastate-interpreter/blob/4fcd2a71f94d6a0e5a1922817dba02e5657558e1/core/js/src/main/scala/sigma/js/GroupElement.scala)
  - [SigmaProp](https://github.com/ScorexFoundation/sigmastate-interpreter/blob/4fcd2a71f94d6a0e5a1922817dba02e5657558e1/core/js/src/main/scala/sigma/js/SigmaProp.scala)
  - [AvlTree](https://github.com/ScorexFoundation/sigmastate-interpreter/blob/4fcd2a71f94d6a0e5a1922817dba02e5657558e1/core/js/src/main/scala/sigma/js/AvlTree.scala)
  
- [sigma-data module](https://github.com/ScorexFoundation/sigmastate-interpreter/tree/b26930c5e7aa58b6d76dda96ab56db59825f8638/data) - contains classes for working with ErgoTree, addresses and all related serializers 
  - all classes from sigma-core module
  - [ErgoTree](https://github.com/ScorexFoundation/sigmastate-interpreter/blob/b26930c5e7aa58b6d76dda96ab56db59825f8638/data/js/src/main/scala/sigma/ast/js/ErgoTree.scala)
  - [Address](https://github.com/ScorexFoundation/sigmastate-interpreter/blob/b745c5fd2257abc6d4317d9761394eb0ea0f3f4e/data/js/src/main/scala/org/ergoplatform/js/Address.scala)
  - [Expr]()
  
- [sigma-interpreter module]() - contains classes for working with ErgoTree interpreter
  - all classes from sigma-data module
  - [ProverHints]()
  - [ProverSecret]()
  - [SigmaPropProver]()
  - [SigmaPropVerifier]()

- [sigma-sdk module](https://github.com/ScorexFoundation/sigmastate-interpreter/tree/6d774a34118b6fac4e70b58c29343afb1b261460/sdk) - contains classes for working with ErgoTree interpreter
  - all classes from sigma-interpreter module
  - [BlockchainParameters](https://github.com/ScorexFoundation/sigmastate-interpreter/blob/ce203cca487c0a2476504f8a11e7a94ba8ef61b5/sdk/js/src/main/scala/org/ergoplatform/sdk/js/BlockchainParameters.scala)
  - [BlockchainStateContext](https://github.com/ScorexFoundation/sigmastate-interpreter/blob/ce203cca487c0a2476504f8a11e7a94ba8ef61b5/sdk/js/src/main/scala/org/ergoplatform/sdk/js/BlockchainStateContext.scala)
  - [ContractTemplate](https://github.com/ScorexFoundation/sigmastate-interpreter/blob/6d774a34118b6fac4e70b58c29343afb1b261460/sdk/js/src/main/scala/org/ergoplatform/sdk/js/ContractTemplate.scala)
  - [Header](https://github.com/ScorexFoundation/sigmastate-interpreter/blob/4fcd2a71f94d6a0e5a1922817dba02e5657558e1/sdk/js/src/main/scala/org/ergoplatform/sdk/js/Header.scala)
  - [PreHeader](https://github.com/ScorexFoundation/sigmastate-interpreter/blob/4fcd2a71f94d6a0e5a1922817dba02e5657558e1/sdk/js/src/main/scala/org/ergoplatform/sdk/js/PreHeader.scala)
  - [ProverBuilder](https://github.com/ScorexFoundation/sigmastate-interpreter/blob/2a77625cd65a39f29fa56aa0e3c9c46cbe038363/sdk/js/src/main/scala/org/ergoplatform/sdk/js/ProverBuilder.scala)
  - [ReducedTransaction](https://github.com/ScorexFoundation/sigmastate-interpreter/blob/fff394ff28ec5530a6535effedd927f2eb297fc0/sdk/js/src/main/scala/org/ergoplatform/sdk/js/ReducedTransaction.scala)
  - [SigmaProver](https://github.com/ScorexFoundation/sigmastate-interpreter/blob/9cdcbde6c77436f154e256c846e8f54aa00bff15/sdk/js/src/main/scala/org/ergoplatform/sdk/js/SigmaProver.scala)

- [sigma-compiler module]() - contains classes for working with ErgoScript compiler
  - [SigmaCompiler](https://github.com/ScorexFoundation/sigmastate-interpreter/blob/aae4118fed18f6587413d9a6330e449b05d8d5ad/sc/js/src/main/scala/sigmastate/lang/js/SigmaCompiler.scala)
  
# Examples

### How to create Sigma type descriptors

Import `TypeObj` module, then use:

- fields to create simple types (e.g. `TypeObj.Int`)
- method `TypeObj.pairType` (e.g. `TypeObj.pairType(TypeObj.Int, TypeObj.Long)`)
- method `TypeObj.collType` (e.g. `TypeObj.collType(TypeObj.Int)`)

See examples in tests [Type.spec.js](https://github.com/ScorexFoundation/sigmastate-interpreter/blob/933acd7a3753725c8b41994c2126a20279b6809b/sigma-js/tests/js/Type.spec.js)

### How to create Sigma values

Import `Value$` module, then use its methods.
See examples in tests [Value.spec.js](https://github.com/ScorexFoundation/sigmastate-interpreter/blob/933acd7a3753725c8b41994c2126a20279b6809b/sigma-js/tests/js/Value.spec.js)

### How to work with ErgoTree

Import `ErgoTree$` module, and `ErgoTree` class then use its methods.
See examples in tests [ErgoTree.spec.js](https://github.com/ScorexFoundation/sigmastate-interpreter/blob/79df4ca171a77233947d835042ce5c82ee520469/sigma-js/tests/js/ErgoTree.spec.js)

### Compile ErgoScript to ErgoTree

Import `SigmaCompilerObj` module and `SigmaCompiler` class, then use its methods.
See compiler tests in [SigmaCompiler.spec.js](https://github.com/ScorexFoundation/sigmastate-interpreter/blob/933acd7a3753725c8b41994c2126a20279b6809b/sigma-js/tests/js/SigmaCompiler.spec.js)
