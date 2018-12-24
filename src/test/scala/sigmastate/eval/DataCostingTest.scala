package sigmastate.eval

import sigmastate.lang.{LangTests, TransformingSigmaBuilder, SigmaCompiler}

import scalan.BaseCtxTests

class DataCostingTest extends BaseCtxTests with LangTests with ErgoScriptTestkit {
  import IR._
  import Col._
  lazy val compiler = new SigmaCompiler(builder)

  test("split cols") {
    emit("split_cols",
      split3(fun { in: Rep[(Col[Int], Byte)] =>
        dataCost(in, None)
      })
    )
  }

//  test("split pair cols") {
//    ctx.emit("split_pair_col",
//      split(fun { in: Rep[(Coll[(Int, Short)], Byte)] =>
//        dataCost(in)
//      })
//    )
//    ctx.emit("split_pair_cols2",
//      split(fun { in: Rep[(Coll[(Int, (Short, Boolean))], Byte)] =>
//        dataCost(in)
//      })
//    )
//  }
//
//  test("split nested cols") {
//    ctx.emit("split_nested_cols",
//      split(fun { in: Rep[(Coll[Coll[Int]], Byte)] =>
//        dataCost(in)
//      })
//    )
//  }
//
//  test("split nested pair cols") {
//    ctx.emit("split_nested_pair_cols",
//      split(fun { in: Rep[(Coll[Coll[(Int, Short)]], Byte)] =>
//        dataCost(in)
//      })
//    )
//  }
//
//  test("split nested nested cols") {
//    ctx.emit("split_nested_nested_cols",
//      split(fun { in: Rep[(Coll[Coll[Coll[Int]]], Byte)] =>
//        dataCost(in)
//      })
//    )
//  }
//
//  test("split nested nested pair cols") {
//    ctx.emit("split_nested_nested_pair_cols",
//      split(fun { in: Rep[(Coll[Coll[Coll[(Int, Short)]]], Byte)] =>
//        dataCost(in)
//      })
//    )
//  }
//
//  test("split complex1 cols") {
//    ctx.emit("split_complex1_cols",
//      split(fun { in: Rep[(Coll[Coll[(Coll[(Int, Short)], Boolean)]], Byte)] =>
//        dataCost(in)
//      })
//    )
//  }
//
//  test("split complex2 cols") {
//    ctx.emit("split_complex2_cols",
//      split(fun { in: Rep[(Coll[(Coll[(Coll[(Int, Boolean)], Short)], Char)], Byte)] =>
//        dataCost(in)
//      })
//    )
//  }
//
}
