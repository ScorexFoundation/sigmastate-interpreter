package sigmastate

import sigmastate.eval.IRContext
import sigmastate.interpreter.Interpreter
import sigmastate.lang.Terms.ValueOps
import special.sigma.SigmaDslTesting
import org.scalacheck.util.Pretty

class PrettyPrintErgoTreeSpecification extends SigmaDslTesting {
  implicit def IR: IRContext = createIR()

  private def compile(code: String, env: Interpreter.ScriptEnv = Interpreter.emptyEnv) = {
    val res = compiler.compile(env, code)
    checkCompilerResult(res)
    res.buildTree.asSigmaProp
  }

  property("booleans"){
    val code = "{ (x: (Int, Boolean)) => ((x._1 == 0) ^ x._2) && (false || true) }"
    val compiledTree = compile(code)
    PrettyPrintErgoTree.prettyPrint(compiledTree) shouldBe
      """{ ($1: (Int, Boolean)) =>
        |  ((($1._1) == (0.toInt)) ^ ($1._2)) && ((false) || (true))
        |}""".stripMargin
  }

  property("reading context of register as typed value"){
    val code = "SELF.R4[Coll[Box]].get"
    val compiledTree = compile(code)
    PrettyPrintErgoTree.prettyPrint(compiledTree) shouldBe code
  }

  property("function with block values in body"){
    val code =
      """{ (x: (Short, Short)) =>
        |  val a = x._1
        |  val b = x._2
        |  val plus = a + b
        |  val minus = a - b
        |  val mul = a * b
        |  val div = a / b
        |  val mod = a % b
        |  (plus, (minus, (mul, (div, mod))))
        |}""".stripMargin
    val compiledTree = compile(code)
    PrettyPrintErgoTree.prettyPrint(compiledTree) shouldBe
      """{ ($1: (Short, Short)) =>
        |  val $3 = $1._1
        |  val $4 = $1._2
        |  ($3 + $4, ($3 - $4, ($3 * $4, ($3 / $4, $3 % $4))))
        |}""".stripMargin
  }

  property("branching"){
    val code =
      """{ (x: Box) =>
        |  val tagOpt = x.R5[Short]
        |  if (tagOpt.isDefined) {
        |    tagOpt.get
        |  } else {
        |    0.toShort
        |  }
        |}""".stripMargin
    val compiledTree = compile(code)
    PrettyPrintErgoTree.prettyPrint(compiledTree, 4) shouldBe
      """{ ($1: Box) =>
        |    val $3 = $1.R5[Short]
        |    if ($3.isDefined) {
        |        $3.get
        |    } else {
        |        0.toShort
        |    }
        |}""".stripMargin
  }

  property("group generator"){
    val code = "{ (x: Int) => groupGenerator }"
    val compiledTree = compile(code)
    PrettyPrintErgoTree.prettyPrint(compiledTree) shouldBe
      """{ ($1: Int) =>
        |  Global.groupGenerator
        |}""".stripMargin
  }

  // TODO: Should be with type param, i.e. `substConstants[Any](...)`?
  property("substConstants"){
    val code = "{ (x: (Coll[Byte], Int)) => substConstants[Any](x._1, Coll[Int](x._2), Coll[Any](sigmaProp(false), sigmaProp(true))) }"
    val compiledTree = compile(code)
    PrettyPrintErgoTree.prettyPrint(compiledTree, 2, 84) shouldBe
      """{ ($1: (Coll[Byte], Int)) =>
        |  substConstants(
        |    $1._1, Coll[Int]($1._2), Coll[Any](sigmaProp(false), sigmaProp(true))
        |  )
        |}""".stripMargin
  }

  property("constant placeholder"){
    import Values.CollectionConstant
    import Values.ByteArrayConstant
    import scorex.util.encode.Base16
    def stringToByteConstant(in: String): CollectionConstant[SByte.type] = ByteArrayConstant(in.getBytes("UTF-8"))
    def decodeString(in: String): CollectionConstant[SByte.type] = ByteArrayConstant(Base16.decode(in).get)

    val expectedResult = decodeString("ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad")
    val calcSha256 = EQ(CalcSha256(stringToByteConstant("abc")), expectedResult)
    val ergoTree = mkTestErgoTree(calcSha256.toSigmaProp)
    // TODO: prettyPrint have to be updated as constant placeholders are not part of ErgoTree root
  }

  property("ergolike nodes"){
    val code = 
      """{ (x: Context) =>
        |  val a = x.OUTPUTS.exists({ (box: Box) => box.value + 5 > 10 })
        |  val b = x.INPUTS.map { (b: Box) => b.value }.getOrElse(5, 0L)
        |  val c = x.HEIGHT
        |  val d = x.LastBlockUtxoRootHash
        |  val e = CONTEXT.dataInputs(0).tokens(0)
        |  (a, (b, (c, (d, e))))
        |}""".stripMargin
    val compiledTree = compile(code)
    PrettyPrintErgoTree.prettyPrint(compiledTree) shouldBe
      """{ ($1: Context) =>
        |  (
        |    OUTPUTS.exists({ ($3: Box) =>
        |      ($3.value + 5.toLong) > (10.toLong)
        |    }), (
        |      INPUTS.map({ ($3: Box) =>
        |        $3.value
        |      }).getOrElse(5.toInt, 0.toLong), (
        |        HEIGHT, (
        |          $1.LastBlockUtxoRootHash, CONTEXT.dataInputs(0.toInt).tokens(0.toInt)
        |        )
        |      )
        |    )
        |  )
        |}""".stripMargin
  }

  property("function application term"){
    val code = 
      """{(x: Option[Long]) =>
        |  def f(opt: Long): Long = opt + 3
        |  if (x.isDefined) f(x.get) else f(5L)
        |}""".stripMargin
    val compiledTree = compile(code)
    PrettyPrintErgoTree.prettyPrint(compiledTree) shouldBe
      """val $1 = { ($1: Long) =>
        |  $1 + 3.toLong
        |}
        |{ ($2: Option[Long]) =>
        |  if ($2.isDefined) {
        |    $1($2.get)
        |  } else {
        |    $1(5.toLong)
        |  }
        |}""".stripMargin
  }

  property("method call with non empty arguments"){
    val code = "OUTPUTS.map({ (b: Box) => b.value }).updateMany(Coll(0), Coll(3L))(0) == 3L"
    val compiledTree = compile(code)
    PrettyPrintErgoTree.prettyPrint(compiledTree) shouldBe
      """(
        |  OUTPUTS.map({ ($1: Box) =>
        |    $1.value
        |  }).updateMany(Coll[Int](0.toInt), Coll[Long](3.toLong))(0.toInt)
        |) == (3.toLong)""".stripMargin
  }

  property("zip nested"){
    val code = 
      """OUTPUTS.zip(INPUTS).zip(OUTPUTS).zip(INPUTS)
        | .map({ (t: (((Box, Box), Box), Box)) =>
        | t._1._2.value + t._2.value
        | }).fold(0L, { (a: Long, v: Long) => a + v }) == 10""".stripMargin
  }
}