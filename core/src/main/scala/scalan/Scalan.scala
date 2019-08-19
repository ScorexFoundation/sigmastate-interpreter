package scalan

import scalan.compilation.GraphVizExport
import scalan.primitives._
import scalan.staged.{Transforming}

/** Aggregate cake with all inter-dependent modules assembled together.
  * Each instance of this class contains independent IR context, thus many
  * instances can be created simultaneously.
  * However, the inner types declared in the traits are path-dependant.
  * This in particular means that ctx1.Ref[_] and ctx2.Ref[_] are different types.
  * The typical usage is to create `val ctx = new Scalan` and then import inner
  * declarations using `import ctx._`.
  * This way the declaration will be directly available as if they were global
  * declarations.
  * At the same time cake design pattern allow to `override` many methods and values
  * in classed derived from `Scalan`, this is significant benefit over
  * *everything is global* design.
  */
class Scalan
  extends TypeDescs
  with MethodCalls
  with Tuples
  with NumericOps
  with UnBinOps
  with LogicalOps
  with OrderingOps
  with Equal
  with UniversalOps
  with Functions
  with IfThenElse
  with Transforming
  with GraphVizExport
  with Thunks
  with Entities
  with Structs
//  with TypeSum
  with Modules
  with DefRewriting

