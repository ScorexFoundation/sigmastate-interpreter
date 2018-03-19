
# Sigma language front-end

Sigma frontend implements the following pipeline:

SourceCode --> `Parser` --> `Binder` -> `Typer` -> `Specializer` -> SigmaIR

Here:
- SourceCode  is a string of unicode characters
- SigmaIR is an intermediate representation which can be processed by Sigma [Interpreter](https://github.com/ScorexFoundation/sigmastate-interpreter/blob/master/src/main/scala/sigmastate/interpreter/Interpreter.scala)
 
## Parser
`SigmaParser` takes a string and produces abstract syntax tree, AST, of a Sigma expression.

In case of any errors it throws `ParserException`

## Binder
`SigmaBinder` takes an AST of successfully parsed Sigma expression and resolves 
global variables and predefined functions be looking up in the provided environment.
Binder transforms objects of predefined Scala types (such as Int, Boolean, Box, etc.)
into constant nodes (IntConstant, BoxConstant, etc) of SigmaIR of the correspodning type.

In case of any error it throws `BinderException`

## Typer
`SigmaTyper` takes a bound AST from the output of `SigmaBinder` and assigns types
to all tree nodes. Since AST is immutable data structure the typer produces a new tree. 

Type assignment is performed in two steps:
1) Computing `env` and `tipe` mutually recursive attributes using 
[Kiama Attribution](https://bitbucket.org/inkytonik/kiama/src/f71e980b74c350a7666dc9f3aa82c155bc79b419/wiki/Attribution.md?fileviewer=file-view-default) package.
2) Using bottom-up rewriting strategy to traverse all the tree nodes to search for nodes 
with unspecified types (signified by NoType object). For every such node rewriter has 
a rule case which replaces NoType with the type computed by `tipe` attribute.

In case of any error it throws `TyperException`

## Specializer

`SigmaSpecializer` takes a typed AST from the output of `SigmaTyper` and performs a final preparation
of AST for interpretation by `Interpreter`. This step is necessary because of the limitations imposed 
by sigma-protocols on SigmaIR which can be processed by `Interpreter`.

The following transformations are performed by `Specializer`:
1) Elimination of Let nodes by substituting its bodes in all places of the referring variables 
(this may lead to duplication of subtrees).
2) Elimination of Lambda nodes by inlining its bodies as the arguments of combinators 
(like Fold, Map, Exists, etc). This may also lead to duplication of subtrees.
3) Replacing of Select nodes with special operations of SigmaIR 
(e.g. box.value --> ExtractAmount(box))
4) Replacement of Apply nodes with special operations of SigmaIR 
(e.g. arr(0) --> ByIndex(arr, 0), etc) 

As result of specialization the following AST nodes should be eliminated:
 Block, Let, Ident, Select, Lambda, Apply 

In case of any error it throws `SpecializerException`
