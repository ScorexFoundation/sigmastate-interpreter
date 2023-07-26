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

Run following command to add Sigma.JS as a project dependency:

```bash
npm install sigmastate-js
```

# Examples

### How to create Sigma type descriptors

Import `TypeObj` module, then use:

- fields to create simple types (e.g. `TypeObj.Int`)
- method `TypeObj.pairType` (e.g. `TypeObj.pairType(TypeObj.Int, TypeObj.Long)`)
- method `TypeObj.collType` (e.g. `TypeObj.collType(TypeObj.Int)`)

See examples in tests [Type.spec.js](https://github.com/ScorexFoundation/sigmastate-interpreter/blob/933acd7a3753725c8b41994c2126a20279b6809b/sigma-js/tests/js/Type.spec.js)

### How to create Sigma values

Import `ValueObj` module, then use its methods.
See examples in tests [Value.spec.js](https://github.com/ScorexFoundation/sigmastate-interpreter/blob/933acd7a3753725c8b41994c2126a20279b6809b/sigma-js/tests/js/Value.spec.js)

### How to work with ErgoTree

Import `ErgoTreeObj` module, and `ErgoTree` class then use its methods.
See examples in tests [ErgoTree.spec.js](https://github.com/ScorexFoundation/sigmastate-interpreter/blob/79df4ca171a77233947d835042ce5c82ee520469/sigma-js/tests/js/ErgoTree.spec.js)

### Compile ErgoScript to ErgoTree

Import `SigmaCompilerObj` module and `SigmaCompiler` class, then use its methods.
See compiler tests in [SigmaCompiler.spec.js](https://github.com/ScorexFoundation/sigmastate-interpreter/blob/933acd7a3753725c8b41994c2126a20279b6809b/sigma-js/tests/js/SigmaCompiler.spec.js)
