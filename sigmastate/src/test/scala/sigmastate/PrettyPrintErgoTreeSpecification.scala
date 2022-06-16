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
        |  val minimum = min(a, b)
        |  val maximum = max(a, b)
        |  (plus, (minus, (mul, (div, (mod, (minimum, maximum))))))
        |}""".stripMargin
    val compiledTree = compile(code)
    PrettyPrintErgoTree.prettyPrint(compiledTree, 100) shouldBe
      """{ ($1: (Short, Short)) =>
        |  val $3 = $1._1; val $4 = $1._2; (
        |    $3 + $4, ($3 - $4, ($3 * $4, ($3 / $4, ($3 % $4, (min($3, $4), max($3, $4))))))
        |  )
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
    PrettyPrintErgoTree.prettyPrint(compiledTree) shouldBe
      """{ ($1: Box) =>
        |  val $3 = $1.R5[Short]; if ($3.isDefined) { $3.get } else { 0.toShort }
        |}""".stripMargin
  }

  property("group generator"){
    val code = "{ (x: Int) => groupGenerator }"
    val compiledTree = compile(code)
    PrettyPrintErgoTree.prettyPrint(compiledTree) shouldBe
      "{ ($1: Int) => Global.groupGenerator }"
  }

  // TODO: Output should be with type param, i.e. `substConstants[Any](...)`? Related to printer config (show explicit types)
  property("substConstants"){
    val code = "{ (x: (Coll[Byte], Int)) => substConstants[Any](x._1, Coll[Int](x._2), Coll[Any](sigmaProp(false), sigmaProp(true))) }"
    val compiledTree = compile(code)
    PrettyPrintErgoTree.prettyPrint(compiledTree, 84) shouldBe
      """{ ($1: (Coll[Byte], Int)) =>
        |  substConstants(
        |    $1._1, Coll[Int]($1._2), Coll[SigmaProp](sigmaProp(false), sigmaProp(true))
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
        |    OUTPUTS.exists({ ($3: Box) => ($3.value + 5.toLong) > (10.toLong) }), (
        |      INPUTS.map({ ($3: Box) => $3.value }).getOrElse(5.toInt, 0.toLong), (
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
      """val $1 = { ($1: Long) => $1 + 3.toLong }; { ($2: Option[Long]) =>
        |  if ($2.isDefined) { $1($2.get) } else { $1(5.toLong) }
        |}""".stripMargin
  }

  property("method call with non empty arguments"){
    val code = "OUTPUTS.map({ (b: Box) => b.value }).updateMany(Coll(0), Coll(3L))(0) == 3L"
    val compiledTree = compile(code)
    PrettyPrintErgoTree.prettyPrint(compiledTree) shouldBe
      """(
        |  OUTPUTS.map({ ($1: Box) => $1.value }).updateMany(
        |    Coll[Int](0.toInt), Coll[Long](3.toLong)
        |  )(0.toInt)
        |) == (3.toLong)""".stripMargin
  }

  property("append"){
    val code = "{ (x: (Coll[Int], Coll[Int])) => x._1.append(x._2) }"
    PrettyPrintErgoTree.prettyPrint(compile(code)) shouldBe
      "{ ($1: (Coll[Int], Coll[Int])) => $1._1.append($1._2) }"
  }

  property("slice"){
    val code = "{ (x: (Coll[Int], (Int, Int))) => x._1.slice(x._2._1, x._2._2) }"
    PrettyPrintErgoTree.prettyPrint(compile(code)) shouldBe
      "{ ($1: (Coll[Int], (Int, Int))) => val $3 = $1._2; $1._1.slice($3._1, $3._2) }"
  }

  property("filter"){
    val code = "{ (x: Coll[Int]) => x.filter({ (v: Int) => v >= 0 }) }"
    PrettyPrintErgoTree.prettyPrint(compile(code)) shouldBe
      "{ ($1: Coll[Int]) => $1.filter({ ($3: Int) => ($3) >= (0.toInt) }) }"
  }

  property("forall"){
    val code = "{ (x: Coll[Box]) => x.forall({(b: Box) => b.value <= 1 }) }"
    PrettyPrintErgoTree.prettyPrint(compile(code)) shouldBe
      "{ ($1: Coll[Box]) => $1.forall({ ($3: Box) => ($3.value) <= (1.toLong) }) }"
  }

  property("fold"){
    val code = "{ (x: (Coll[Byte], Int)) => x._1.fold(x._2, { (i1: Int, i2: Byte) => i1 + i2 }) }"
    PrettyPrintErgoTree.prettyPrint(compile(code)) shouldBe
      """{ ($1: (Coll[Byte], Int)) =>
        |  $1._1.fold($1._2, { ($3: (Int, Byte)) => $3._1 + $3._2.toInt })
        |}""".stripMargin
  }

  property("propBytes"){
    val code = "{ (x: SigmaProp) => x.propBytes }"
    PrettyPrintErgoTree.prettyPrint(compile(code)) shouldBe
      "{ ($1: SigmaProp) => $1.propBytes }"
  }

  property("size of"){
    val code = "{ (x: Coll[Box]) => x.size }"
    PrettyPrintErgoTree.prettyPrint(compile(code)) shouldBe
      "{ ($1: Coll[Box]) => $1.size }"
  }

  property("extract script bytes"){
    val code = "{ (x: Box) => x.propositionBytes }"
    PrettyPrintErgoTree.prettyPrint(compile(code)) shouldBe
      "{ ($1: Box) => $1.propositionBytes }"
  }

  property("extract bytes"){
    val code = "{ (x: Box) => x.bytes }"
    PrettyPrintErgoTree.prettyPrint(compile(code)) shouldBe
      "{ ($1: Box) => $1.bytes }"
  }

  property("extract bytes with no ref"){
    val code = "{ (x: Box) => x.bytesWithoutRef }"
    PrettyPrintErgoTree.prettyPrint(compile(code)) shouldBe
      "{ ($1: Box) => $1.bytesWithoutRef }"
  }

  property("extract id"){
    val code = "{ (x: Box) => x.id }"
    PrettyPrintErgoTree.prettyPrint(compile(code)) shouldBe
      "{ ($1: Box) => $1.id }"
  }

  property("extract creation info"){
    val code = "{ (x: Box) => x.creationInfo }"
    PrettyPrintErgoTree.prettyPrint(compile(code)) shouldBe
      "{ ($1: Box) => $1.creationInfo }"
  }

  property("getVar"){
    val code = "{ (x: Context) => getVar[Boolean](11) }"
    PrettyPrintErgoTree.prettyPrint(compile(code)) shouldBe
      "{ ($1: Context) => getVar[Boolean](11.toByte) }"
  }

  property("getOrElse"){
    val code = "{ (x: Option[Long]) => x.getOrElse(1L) }"
    PrettyPrintErgoTree.prettyPrint(compile(code)) shouldBe
      "{ ($1: Option[Long]) => $1.getOrElse(1.toLong) }"
  }

  property("proveDlog"){
    val code = "{ (x: GroupElement) => proveDlog(x) }"
    PrettyPrintErgoTree.prettyPrint(compile(code)) shouldBe
      "{ ($1: GroupElement) => proveDlog($1) }"
  }

  property("proveDHTuple"){
    val code = "{ (x: GroupElement) => proveDHTuple(x, x, x, x) }"
    PrettyPrintErgoTree.prettyPrint(compile(code)) shouldBe
      "{ ($1: GroupElement) => proveDHTuple($1, $1, $1, $1) }"
  }
  
  property("sigma and"){
    val code = "{ (x:(SigmaProp, SigmaProp)) => x._1 && x._2 }"
    PrettyPrintErgoTree.prettyPrint(compile(code)) shouldBe
      "{ ($1: (SigmaProp, SigmaProp)) => $1._1 && $1._2 }"
  }

  property("sigma or"){
    val code = "{ (x:(SigmaProp, SigmaProp)) => x._1 || x._2 }"
    PrettyPrintErgoTree.prettyPrint(compile(code)) shouldBe
      "{ ($1: (SigmaProp, SigmaProp)) => $1._1 || $1._2 }"
  }

  property("logical or for collections (anyOf)"){
    val code = "{ (x: Coll[Boolean]) => anyOf(x) }"
    PrettyPrintErgoTree.prettyPrint(compile(code)) shouldBe
      "{ ($1: Coll[Boolean]) => anyOf($1) }"
  }

  property("logical xor for collections (xorOf)"){
    val code = "{ (x: Coll[Boolean]) => xorOf(x) }"
    PrettyPrintErgoTree.prettyPrint(compile(code)) shouldBe
      "{ ($1: Coll[Boolean]) => xorOf($1) }"
  }

  property("logical and for collections (allOr)"){
    val code = "{ (x: Coll[Boolean]) => allOf(x) }"
    PrettyPrintErgoTree.prettyPrint(compile(code)) shouldBe
      "{ ($1: Coll[Boolean]) => allOf($1) }"
  }

  property("atLeast"){
    val code = "{ (x: Coll[SigmaProp]) => atLeast(x.size - 1, x) }"
    PrettyPrintErgoTree.prettyPrint(compile(code)) shouldBe
      "{ ($1: Coll[SigmaProp]) => atLeast($1.size - 1.toInt, $1) }"
  }

  property("downcast"){
    val code = "{ (x: Short) => x.toByte }"
    PrettyPrintErgoTree.prettyPrint(compile(code)) shouldBe
      "{ ($1: Short) => $1.toByte }"
  }
  
  property("longToByteArray"){
    val code = "{ (x: Long) => longToByteArray(x) }"
    PrettyPrintErgoTree.prettyPrint(compile(code)) shouldBe
      "{ ($1: Long) => longToByteArray($1) }"
  }

  property("byteArrayToLong"){
    val code = "{ (x: Coll[Byte]) => byteArrayToLong(x) }"
    PrettyPrintErgoTree.prettyPrint(compile(code)) shouldBe
      "{ ($1: Coll[Byte]) => byteArrayToLong($1) }"
  }

  property("byteArrayToBigInt"){
    val code = "{ (x: Coll[Byte]) => byteArrayToBigInt(x) }"
    PrettyPrintErgoTree.prettyPrint(compile(code)) shouldBe
      "{ ($1: Coll[Byte]) => byteArrayToBigInt($1) }"
  }

  property("decode point"){
    val code = "{ (x: GroupElement) => decodePoint(x.getEncoded) == x }"
    PrettyPrintErgoTree.prettyPrint(compile(code)) shouldBe
      "{ ($1: GroupElement) => (decodePoint($1.getEncoded)) == ($1) }"
  }
  
  property("blake2b256"){
    val code = "{ (x: Coll[Byte]) => blake2b256(x) }"
    PrettyPrintErgoTree.prettyPrint(compile(code)) shouldBe
      "{ ($1: Coll[Byte]) => blake2b256($1) }"
  }
  
  property("sha256"){
    val code = "{ (x: Coll[Byte]) => sha256(x) }"
    PrettyPrintErgoTree.prettyPrint(compile(code)) shouldBe
      "{ ($1: Coll[Byte]) => sha256($1) }"
  }

  // TODO: Uncomment after https://github.com/ScorexFoundation/sigmastate-interpreter/issues/474
  ignore("bitwise operations"){
    val code =
      """{ (x: (Int, Int)) =>
        |  val a = x._1
        |  val b = x._2
        |  val and = a & b
        |  val or = a | b
        |  val xor = a ^ b
        |  val shiftRight = a >> b
        |  val shiftLeft = a << b
        |  val shiftRightZeroed = a >>> b
        |  (and, (or, (xor, (shiftRight, (shiftLeft, shiftRightZeroed)))))
        |}""".stripMargin
    val compiledTree = compile(code)
    PrettyPrintErgoTree.prettyPrint(compiledTree) shouldBe
      """{ ($1: (Int, Int)) =>
        |  val $3 = $1._1
        |  val $4 = $1._2
        |  ($3 & $4, ($3 | $4, ($3 ^ $4, ($3 >> $4, ($3 << $4, $3 >>> $4)))))
        |}""".stripMargin
  }

  property("xor for byte arrays"){
    val code = "{ (x: (Coll[Byte], Coll[Byte])) => xor(x._1, x._2) }"
    PrettyPrintErgoTree.prettyPrint(compile(code)) shouldBe
      "{ ($1: (Coll[Byte], Coll[Byte])) => xor($1._1, $1._2) }"
  }

  property("exponentiate group"){
    val code = "{ (x: (GroupElement, BigInt)) => x._1.exp(x._2) }"
    PrettyPrintErgoTree.prettyPrint(compile(code)) shouldBe
      "{ ($1: (GroupElement, BigInt)) => $1._1.exp($1._2) }"
  }

  property("multiply group"){
    val code = "{ (x: (GroupElement, GroupElement)) => x._1.multiply(x._2) }"
    PrettyPrintErgoTree.prettyPrint(compile(code)) shouldBe
      "{ ($1: (GroupElement, GroupElement)) => $1._1.multiply($1._2) }"
  }

  property("neq"){
    val code = 
      """{ (x: Context) =>
        |  x.dataInputs(0).R4[Coll[Byte]].get != x.SELF.propositionBytes
        |}""".stripMargin
    PrettyPrintErgoTree.prettyPrint(compile(code)) shouldBe
      """{ ($1: Context) =>
         |  ($1.dataInputs(0.toInt).R4[Coll[Byte]].get) != (SELF.propositionBytes)
         |}""".stripMargin
  }

  property("logical not"){
    val code = "{ (x: Boolean) => !x }"
    PrettyPrintErgoTree.prettyPrint(compile(code)) shouldBe
      "{ ($1: Boolean) => !$1 }"
  }

  ignore("bit inversion"){
    val code = "{ (x: Int) => ~x }"
    PrettyPrintErgoTree.prettyPrint(compile(code)) shouldBe
      "{ ($1: Int) => ~$1 }"
  }

  property("negation"){
    val code = "{ (x: Byte) => -x }"
    PrettyPrintErgoTree.prettyPrint(compile(code)) shouldBe
      "{ ($1: Byte) => !$1 }"
  }

  property("pre header"){
    val code = "{ (x: PreHeader) => x.height }"
    PrettyPrintErgoTree.prettyPrint(compile(code)) shouldBe
      "{ ($1: PreHeader) => $1.height }"
  }

  property("nested ergo script"){
    val code = 
      """OUTPUTS.zip(INPUTS).zip(OUTPUTS).zip(INPUTS)
        | .map({ (t: (((Box, Box), Box), Box)) =>
        | t._1._2.value + t._2.value
        | }).fold(0L, { (a: Long, v: Long) => a + v }) == 10""".stripMargin
    // TODO: $1 is shadowed, look closer to fold: possible problem with clash for Long/Box values, consider having unique global variable table?
    PrettyPrintErgoTree.prettyPrint(compile(code)) shouldBe
      """|(
         |  OUTPUTS.zip(INPUTS).zip(OUTPUTS).zip(INPUTS).map(
         |    { ($1: (((Box, Box), Box), Box)) => $1._1._2.value + $1._2.value }
         |  ).fold(0.toLong, { ($1: (Long, Long)) => $1._1 + $1._2 })
         |) == (10.toLong)""".stripMargin
  }

  property("various types in function definition"){
    val code =
      """{ (x: (String, (Any, (AvlTree, Header)))) =>
         |  false
         |}""".stripMargin
    PrettyPrintErgoTree.prettyPrint(compile(code)) shouldBe
      "{ ($1: (String, (Any, (AvlTree, Header)))) => false }"
  }
}