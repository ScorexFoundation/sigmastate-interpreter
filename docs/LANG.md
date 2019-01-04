
# Sigma language front-end

Sigma frontend implements the following pipeline:

SourceCode --> `Parser` --> `Binder` -> `Typer` -> `CompiletimeCosting` -> `TreeBuilding` -> ErgoTree

Here:
- SourceCode  is a string of unicode characters
- ErgoTree is an intermediate representation which can be processed by Sigma [Interpreter](https://github.com/ScorexFoundation/sigmastate-interpreter/blob/master/src/main/scala/sigmastate/interpreter/Interpreter.scala)
 
## Parser
`SigmaParser` takes a string and produces abstract syntax tree, AST, of a Sigma expression represent by `Value` type in Scala.

In case of any errors it throws `ParserException`

## Binder
`SigmaBinder` takes an AST of successfully parsed Sigma expression and resolves 
global variables and predefined functions be looking up in the provided environment.
Binder transforms environment values of predefined Scala types (such as Int, Boolean, Box, etc.)
into constant nodes (IntConstant, BoxConstant, etc) of the corresponding type. (See also `Constant` class)

In case of any error it throws `BinderException`

## Typer
`SigmaTyper` takes an AST from the output of `SigmaBinder` and assigns types
to all tree nodes. Since AST is immutable data structure the typer produces a new tree. 

Type assignment is performed by `assignType` tree transformation which assign correct types for all 
tree nodes.

In case of any error it throws `TyperException`

## Costing

See Costing.md for detailed description of costing process. 

## TreeBuilding

 
## IR contexts
There are following IR contexts.

Context class         | Description
----------------------|------------
 IRContext            | Generic context which includes extends Evaluation, RuntimeCosting and TreeBuilding
 RuntimeIRContext     | context which should be used during transaction validation
 CompiletimeIRContext | context which should be used during ErgoScript compilation

The reason to have different contexts is to limit RuntimeIRContext to support only those nodes which can be serialized as part of ErgoTree.
This doesn't include all ErgoScript nodes which CompiletimeIRContext can handle.
For example, compileWithCosting should use IR context with CompiletimeCosting mixed in.
However, Interpreter takes as input compiled or deserialized ErgoTree, so it can work without CompiletimeCosting mixed in into IR context.


