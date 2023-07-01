[![CI](https://github.com/ScorexFoundation/sigmastate-interpreter/actions/workflows/ci.yml/badge.svg)](https://github.com/ScorexFoundation/sigmastate-interpreter/actions/workflows/ci.yml)
[![codecov](https://codecov.io/gh/ScorexFoundation/sigmastate-interpreter/branch/develop/graph/badge.svg?token=HNu2ZEOoV6)](https://codecov.io/gh/ScorexFoundation/sigmastate-interpreter)

# ErgoScript compiler and ErgoTree interpreter

This repository contains implementations of ErgoScript compiler and ErgoTree
Interpreter for a family of Sigma-protocol based authentication languages (or simply
Sigma language).

This JS package is cross-compiled from [Scala
implementation](https://github.com/ScorexFoundation/sigmastate-interpreter) using Scala.js
compiler.

The modules published here can be used directly from JavaScript.

# Getting Started

Add the following dependency to your `package.json`:

```json
{
  "dependencies": {
    "sigmastate-js": "0.1.1"
  }
}
```
Then run `npm install`.

# Examples

### How to create Sigma type descriptors

Import `TypeObj` module, then use:
- fields to create simple types (e.g. `TypeObj.Int`)
- method `TypeObj.pairType` (e.g. `TypeObj.pairType(TypeObj.Int, TypeObj.Long)`)
- method `TypeObj.collType` (e.g. `TypeObj.collType(TypeObj.Int)`)
  
See examples in tests [sdk.spec.js](tests/js/sdk.spec.js)

### How to create Sigma values

Import `ValueObj` module, then use its methods.
See examples in tests [sdk.spec.js](tests/js/sdk.spec.js)

### How to work with ErgoTree

Import `ErgoTreeObj` module, and `ErgoTree` class then use its methods.
See examples in tests [sdk.spec.js](tests/js/sdk.spec.js)

### Compile ErgoScript to ErgoTree

Import `SigmaCompilerObj` module and `SigmaCompiler` class, then use its methods.
See compiler tests in [sc.spec.js](tests/js/sc.spec.js)

