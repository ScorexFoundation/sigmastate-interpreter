package sigmastate

import org.ergoplatform.MinerPubkey
import org.ergoplatform.settings.ErgoAlgos
import sigmastate.eval.IRContext
import sigmastate.interpreter.Interpreter
import sigmastate.serialization.ErgoTreeSerializer.DefaultSerializer
import sigmastate.utxo.SelectField
import sigmastate.Values.{BigIntConstant, BlockValue, ByteArrayConstant, GroupGenerator, SValue, Tuple, ValDef, ValUse}
import special.sigma.SigmaDslTesting

class PrettyPrintErgoTreeSpecification extends SigmaDslTesting {
  implicit def IR: IRContext = createIR()
  private def compile(code: String, env: Interpreter.ScriptEnv = Interpreter.emptyEnv): SValue = compile(env, code)

  property("block value"){
    val node = BlockValue(
      Array(
        ValDef(1, List(), Tuple(1, 5)),
        ValDef(3, List(), SelectField(ValUse(1, STuple(SInt, SInt)), 1)),
        ValDef(4, List(), SelectField(ValUse(1, STuple(SInt, SInt)), 2))
      ),
      Plus(ValUse(3, SInt), ValUse(4, SInt))
    )
    PrettyPrintErgoTree.prettyPrint(node) shouldBe
      """{
        |  val tuple1 = (1, 5)
        |  val i3 = tuple1._1
        |  val i4 = tuple1._2
        |  i3 + i4
        |}""".stripMargin
  }

  property("booleans"){
    val code = "{ (x: (Int, Boolean)) => ((x._1 == 0) ^ x._2) && (false || true) }"
    val compiledTree = compile(code)
    PrettyPrintErgoTree.prettyPrint(compiledTree, 60) shouldBe
      """{{(tuple1: (Int, Boolean)) =>
        |  ((tuple1._1 == 0) ^ tuple1._2) && (false || true)
        |}}""".stripMargin
  }

  property("reading context of register as typed value"){
    val code = "{SELF.R4[Coll[Box]].get}"
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
      """{{(tuple1: (Short, Short)) =>
        |  val s3 = tuple1._1
        |  val s4 = tuple1._2
        |  (s3 + s4, (s3 - s4, (s3 * s4, (s3 / s4, (s3 % s4, (min(s3, s4), max(s3, s4)))))))
        |}}""".stripMargin
    
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
    PrettyPrintErgoTree.prettyPrint(compiledTree, 80, 4) shouldBe
      """{{(box1: Box) =>
        |    val opt3 = box1.R5[Short]
        |    if (opt3.isDefined) { opt3.get } else { 0.toShort }
        |}}""".stripMargin
  }

  property("group generator"){
    val code = "{ (x: Int) => groupGenerator }"
    val compiledTree = compile(code)
    PrettyPrintErgoTree.prettyPrint(compiledTree) shouldBe
      "{{(i1: Int) => Global.groupGenerator }}"
  }

  property("substConstants"){
    val code = "{ (x: (Coll[Byte], Int)) => substConstants[Any](x._1, Coll[Int](x._2), Coll[Any](sigmaProp(false), sigmaProp(true))) }"
    val compiledTree = compile(code)
    PrettyPrintErgoTree.prettyPrint(compiledTree, 84) shouldBe
      """{{(tuple1: (Coll[Byte], Int)) =>
        |  substConstants(
        |    tuple1._1, Coll[Int](tuple1._2), Coll[SigmaProp](
        |      sigmaProp(false), sigmaProp(true)
        |    )
        |  )
        |}}""".stripMargin
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
    ergoTree.root match {
      case Left(value) => ???
      case Right(value) =>
        PrettyPrintErgoTree.prettyPrint(value, 70) shouldBe 
          """{sigmaProp(
            |  sha256(placeholder[Coll[Byte]](0)) == placeholder[Coll[Byte]](1)
            |)}""".stripMargin
    }
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
    PrettyPrintErgoTree.prettyPrint(compiledTree, 100) shouldBe
      """{{(ctx1: Context) =>
        |  (
        |    OUTPUTS.exists({(box3: Box) => box3.value + 5L > 10L }), (
        |      INPUTS.map({(box3: Box) => box3.value }).getOrElse(5, 0L), (
        |        HEIGHT, (ctx1.LastBlockUtxoRootHash, CONTEXT.dataInputs(0).tokens(0))
        |      )
        |    )
        |  )
        |}}""".stripMargin
  }

  property("function application term"){
    val code = 
      """{(x: Option[Long]) =>
        |  def f(opt: Long): Long = opt + 3
        |  if (x.isDefined) f(x.get) else f(5L)
        |}""".stripMargin
    val compiledTree = compile(code)
    PrettyPrintErgoTree.prettyPrint(compiledTree) shouldBe
      """{
        |  val func1 = {(l1: Long) => l1 + 3L }
        |  {(opt2: Option[Long]) =>
        |    if (opt2.isDefined) { func1(opt2.get) } else { func1(5L) }
        |  }
        |}""".stripMargin
  }

  property("method call with non empty arguments"){
    val code = "OUTPUTS.map({ (b: Box) => b.value }).updateMany(Coll(0), Coll(3L))(0) == 3L"
    val compiledTree = compile(code)
    PrettyPrintErgoTree.prettyPrint(compiledTree) shouldBe
      """{OUTPUTS.map({(box1: Box) => box1.value }).updateMany(
        |  Coll[Int](0), Coll[Long](3L)
        |)(0) == 3L}""".stripMargin
  }

  property("append"){
    val code = "{ (x: (Coll[Int], Coll[Int])) => x._1.append(x._2) }"
    PrettyPrintErgoTree.prettyPrint(compile(code)) shouldBe
      "{{(tuple1: (Coll[Int], Coll[Int])) => tuple1._1.append(tuple1._2) }}"
  }

  property("slice"){ 
    val code = "{ (x: (Coll[Int], (Int, Int))) => x._1.slice(x._2._1, x._2._2) }"
    PrettyPrintErgoTree.prettyPrint(compile(code)) shouldBe
      """{{(tuple1: (Coll[Int], (Int, Int))) =>
        |  val tuple3 = tuple1._2
        |  tuple1._1.slice(tuple3._1, tuple3._2)
        |}}""".stripMargin
  }

  property("filter"){
    val code = "{ (x: Coll[Int]) => x.filter({ (v: Int) => v >= 0 }) }"
    PrettyPrintErgoTree.prettyPrint(compile(code)) shouldBe
      "{{(coll1: Coll[Int]) => coll1.filter({(i3: Int) => i3 >= 0 }) }}"
  }

  property("forall"){
    val code = "{ (x: Coll[Box]) => x.forall({(b: Box) => b.value <= 1 }) }"
    PrettyPrintErgoTree.prettyPrint(compile(code)) shouldBe
      "{{(coll1: Coll[Box]) => coll1.forall({(box3: Box) => box3.value <= 1L }) }}"
  }

  property("fold"){
    val code = "{ (x: (Coll[Byte], Int)) => x._1.fold(x._2, { (i1: Int, i2: Byte) => i1 + i2 }) }"
    PrettyPrintErgoTree.prettyPrint(compile(code)) shouldBe
      """{{(tuple1: (Coll[Byte], Int)) =>
        |  tuple1._1.fold(
        |    tuple1._2, {(tuple3: (Int, Byte)) => tuple3._1 + tuple3._2.toInt }
        |  )
        |}}""".stripMargin
  }

  property("propBytes"){
    val code = "{ (x: SigmaProp) => x.propBytes }"
    PrettyPrintErgoTree.prettyPrint(compile(code)) shouldBe
      "{{(prop1: SigmaProp) => prop1.propBytes }}"
  }

  property("size of"){
    val code = "{ (x: Coll[Box]) => x.size }"
    PrettyPrintErgoTree.prettyPrint(compile(code)) shouldBe
      "{{(coll1: Coll[Box]) => coll1.size }}"
  }

  property("extract script bytes"){
    val code = "{ (x: Box) => x.propositionBytes }"
    PrettyPrintErgoTree.prettyPrint(compile(code)) shouldBe
      "{{(box1: Box) => box1.propositionBytes }}"
  }

  property("extract bytes"){
    val code = "{ (x: Box) => x.bytes }"
    PrettyPrintErgoTree.prettyPrint(compile(code)) shouldBe
      "{{(box1: Box) => box1.bytes }}"
  }

  property("extract bytes with no ref"){
    val code = "{ (x: Box) => x.bytesWithoutRef }"
    PrettyPrintErgoTree.prettyPrint(compile(code)) shouldBe
      "{{(box1: Box) => box1.bytesWithoutRef }}"
  }

  property("extract id"){
    val code = "{ (x: Box) => x.id }"
    PrettyPrintErgoTree.prettyPrint(compile(code)) shouldBe
      "{{(box1: Box) => box1.id }}"
  }

  property("extract creation info"){
    val code = "{ (x: Box) => x.creationInfo }"
    PrettyPrintErgoTree.prettyPrint(compile(code)) shouldBe
      "{{(box1: Box) => box1.creationInfo }}"
  }

  property("getVar"){
    val code = "{ (x: Context) => getVar[Boolean](11) }"
    PrettyPrintErgoTree.prettyPrint(compile(code)) shouldBe
      "{{(ctx1: Context) => getVar[Boolean](11.toByte) }}"
  }

  property("getOrElse"){
    val code = "{ (x: Option[Long]) => x.getOrElse(1L) }"
    PrettyPrintErgoTree.prettyPrint(compile(code)) shouldBe
      "{{(opt1: Option[Long]) => opt1.getOrElse(1L) }}"
  }

  property("proveDlog"){
    val code = "{ (x: GroupElement) => proveDlog(x) }"
    PrettyPrintErgoTree.prettyPrint(compile(code)) shouldBe
      "{{(ge1: GroupElement) => proveDlog(ge1) }}"
  }

  property("proveDHTuple"){
    val code = "{ (x: GroupElement) => proveDHTuple(x, x, x, x) }"
    PrettyPrintErgoTree.prettyPrint(compile(code)) shouldBe
      "{{(ge1: GroupElement) => proveDHTuple(ge1, ge1, ge1, ge1) }}"
  }
  
  property("sigma and"){
    val code = "{ (x:(SigmaProp, SigmaProp)) => x._1 && x._2 }"
    PrettyPrintErgoTree.prettyPrint(compile(code)) shouldBe
      "{{(tuple1: (SigmaProp, SigmaProp)) => tuple1._1 && tuple1._2 }}"
  }

  property("sigma or"){
    val code = "{ (x:(SigmaProp, SigmaProp)) => x._1 || x._2 }"
    PrettyPrintErgoTree.prettyPrint(compile(code)) shouldBe
      "{{(tuple1: (SigmaProp, SigmaProp)) => tuple1._1 || tuple1._2 }}"
  }

  property("logical or for collections (anyOf)"){
    val code = "{ (x: Coll[Boolean]) => anyOf(x) }"
    PrettyPrintErgoTree.prettyPrint(compile(code)) shouldBe
      "{{(coll1: Coll[Boolean]) => anyOf(coll1) }}"
  }

  property("logical xor for collections (xorOf)"){
    val code = "{ (x: Coll[Boolean]) => xorOf(x) }"
    PrettyPrintErgoTree.prettyPrint(compile(code)) shouldBe
      "{{(coll1: Coll[Boolean]) => xorOf(coll1) }}"
  }

  property("logical and for collections (allOr)"){
    val code = "{ (x: Coll[Boolean]) => allOf(x) }"
    PrettyPrintErgoTree.prettyPrint(compile(code)) shouldBe
      "{{(coll1: Coll[Boolean]) => allOf(coll1) }}"
  }

  property("atLeast"){
    val code = "{ (x: Coll[SigmaProp]) => atLeast(x.size - 1, x) }"
    PrettyPrintErgoTree.prettyPrint(compile(code)) shouldBe
      "{{(coll1: Coll[SigmaProp]) => atLeast(coll1.size - 1, coll1) }}"
  }

  property("downcast"){
    val code = "{ (x: Short) => x.toByte }"
    PrettyPrintErgoTree.prettyPrint(compile(code)) shouldBe
      "{{(s1: Short) => s1.toByte }}"
  }
  
  property("longToByteArray"){
    val code = "{ (x: Long) => longToByteArray(x) }"
    PrettyPrintErgoTree.prettyPrint(compile(code)) shouldBe
      "{{(l1: Long) => longToByteArray(l1) }}"
  }

  property("byteArrayToLong"){
    val code = "{ (x: Coll[Byte]) => byteArrayToLong(x) }"
    PrettyPrintErgoTree.prettyPrint(compile(code)) shouldBe
      "{{(coll1: Coll[Byte]) => byteArrayToLong(coll1) }}"
  }

  property("byteArrayToBigInt"){
    val code = "{ (x: Coll[Byte]) => byteArrayToBigInt(x) }"
    PrettyPrintErgoTree.prettyPrint(compile(code)) shouldBe
      "{{(coll1: Coll[Byte]) => byteArrayToBigInt(coll1) }}"
  }

  property("decode point"){
    val code = "{ (x: GroupElement) => decodePoint(x.getEncoded) == x }"
    PrettyPrintErgoTree.prettyPrint(compile(code)) shouldBe
      "{{(ge1: GroupElement) => decodePoint(ge1.getEncoded) == ge1 }}"
  }
  
  property("blake2b256"){
    val code = "{ (x: Coll[Byte]) => blake2b256(x) }"
    PrettyPrintErgoTree.prettyPrint(compile(code)) shouldBe
      "{{(coll1: Coll[Byte]) => blake2b256(coll1) }}"
  }
  
  property("sha256"){
    val code = "{ (x: Coll[Byte]) => sha256(x) }"
    PrettyPrintErgoTree.prettyPrint(compile(code)) shouldBe
      "{{(coll1: Coll[Byte]) => sha256(coll1) }}"
  }

  // Related to test below - where binary operations are tested
  ignore("bit inversion"){
    val code = "{ (x: Int) => ~x }"
    PrettyPrintErgoTree.prettyPrint(compile(code)) shouldBe
      "{{(i1: Int) => ~i1 }}"
  }

  // Uncomment after https://github.com/ScorexFoundation/sigmastate-interpreter/issues/474
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
      """{{($1: (Int, Int)) =>
        |  val $3 = $1._1
        |  val $4 = $1._2
        |  ($3 & $4, ($3 | $4, ($3 ^ $4, ($3 >> $4, ($3 << $4, $3 >>> $4)))))
        |}}""".stripMargin
  }

  property("xor for byte arrays"){
    val code = "{ (x: (Coll[Byte], Coll[Byte])) => xor(x._1, x._2) }"
    PrettyPrintErgoTree.prettyPrint(compile(code)) shouldBe
      "{{(tuple1: (Coll[Byte], Coll[Byte])) => xor(tuple1._1, tuple1._2) }}"
  }

  property("exponentiate group"){
    val code = "{ (x: (GroupElement, BigInt)) => x._1.exp(x._2) }"
    PrettyPrintErgoTree.prettyPrint(compile(code)) shouldBe
      "{{(tuple1: (GroupElement, BigInt)) => tuple1._1.exp(tuple1._2) }}"
  }

  property("multiply group"){
    val code = "{ (x: (GroupElement, GroupElement)) => x._1.multiply(x._2) }"
    PrettyPrintErgoTree.prettyPrint(compile(code)) shouldBe
      "{{(tuple1: (GroupElement, GroupElement)) => tuple1._1.multiply(tuple1._2) }}"
  }

  property("neq"){
    val code = 
      """{ (x: Context) =>
        |  x.dataInputs(0).R4[Coll[Byte]].get != x.SELF.propositionBytes
        |}""".stripMargin
    PrettyPrintErgoTree.prettyPrint(compile(code)) shouldBe
      """{{(ctx1: Context) =>
         |  ctx1.dataInputs(0).R4[Coll[Byte]].get != SELF.propositionBytes
         |}}""".stripMargin
  }

  property("logical not"){
    val code = "{ (x: Boolean) => !x }"
    PrettyPrintErgoTree.prettyPrint(compile(code)) shouldBe
      "{{(bool1: Boolean) => !bool1 }}"
  }

  property("negation"){
    val code = "{ (x: Byte) => -x }"
    PrettyPrintErgoTree.prettyPrint(compile(code)) shouldBe
      "{{(b1: Byte) => !b1 }}"
  }

  property("pre header"){
    val code = "{ (x: PreHeader) => x.height }"
    PrettyPrintErgoTree.prettyPrint(compile(code)) shouldBe
      "{{(preHeader1: PreHeader) => preHeader1.height }}"
  }

  property("nested ergo script"){
    val code = 
      """OUTPUTS.zip(INPUTS).zip(OUTPUTS).zip(INPUTS)
        | .map({ (t: (((Box, Box), Box), Box)) =>
        |   t._1._2.value + t._2.value
        | }).fold(0L, { (a: Long, v: Long) => a + v }) == 10""".stripMargin
    PrettyPrintErgoTree.prettyPrint(compile(code)) shouldBe
      """{OUTPUTS.zip(INPUTS).zip(OUTPUTS).zip(INPUTS).map(
         |  {(tuple1: (((Box, Box), Box), Box)) => tuple1._1._2.value + tuple1._2.value }
         |).fold(0L, {(tuple1: (Long, Long)) => tuple1._1 + tuple1._2 }) == 10L}""".stripMargin
  }

  property("various types in function definition"){
    val code =
      """{ (x: (String, (Any, (AvlTree, (Header, Unit))))) =>
         |  false
         |}""".stripMargin
    PrettyPrintErgoTree.prettyPrint(compile(code)) shouldBe
      "{{(tuple1: (String, (Any, (AvlTree, (Header, Unit))))) => false }}"
  }

  // TODO: This doesn't look right
  property("deserialize"){
    import scorex.util.encode.Base58
    import sigmastate.serialization.ValueSerializer

    val str = Base58.encode(ValueSerializer.serialize(ByteArrayConstant(Array[Byte](2))))
    val code = s"""deserialize[Coll[Byte]]("$str")(0) == 2"""
    PrettyPrintErgoTree.prettyPrint(compile(code)) shouldBe
      """{Coll[Byte](2)(0).toInt == 2}"""
  }

  property("explicit parenthesis for boolean operations"){
    val code = "{ (x: Boolean) => !(x && (true || !true)) }"
    PrettyPrintErgoTree.prettyPrint(compile(code)) shouldBe
      "{{(bool1: Boolean) => !(bool1 && (true || false)) }}"
  }

  property("group generator created explicitly with ErgoTree"){
    val equalNode = EQ(
      Exponentiate(GroupGenerator, BigIntConstant(1)),
      Exponentiate(GroupGenerator, BigIntConstant(6))
    )
    val ergoTree = Values.ErgoTree.withoutSegregation(equalNode.toSigmaProp)
    ergoTree.root match {
      case Left(value) => ???
      case Right(value) => PrettyPrintErgoTree.prettyPrint(value, 70) shouldBe
        """{sigmaProp(
          |  groupGenerator.exp(1.toBigInt) == groupGenerator.exp(6.toBigInt)
          |)}""".stripMargin
    }
  }

  // TODO: How to create byte array in ergoscript so output produces Coll[Byte]? Now the ouptput doesn't compile.
  property("minerPubKey"){
    val equalNode = EQ(MinerPubkey, ByteArrayConstant(Array[Byte](2, 10)))
    val ergoTree = Values.ErgoTree.withoutSegregation(equalNode.toSigmaProp)
    ergoTree.root match {
      case Left(value) => ???
      case Right(value) => PrettyPrintErgoTree.prettyPrint(value) shouldBe
        """{sigmaProp(minerPubKey == Coll[Byte](2,10))}"""
    }
  }

  property("global created explicitly with ErgoTree"){
    val node = 
      Values.BlockValue(
        Array(Values.ValDef(1, List(), Values.FuncValue(Array((1,SGlobal)), Values.FalseLeaf))),
        BinAnd(Values.TrueLeaf, Values.FalseLeaf)
      )
    val ergoTree = Values.ErgoTree.withoutSegregation(node.toSigmaProp)
    ergoTree.root match {
      case Left(value) => ???
      case Right(value) =>
        PrettyPrintErgoTree.prettyPrint(value) shouldBe
        """{sigmaProp(
          |  val func1 = {(global1: Global) => false }
          |  true && false
          |)}""".stripMargin
    }      
  }

  ignore("func created explicitly with ErgoTree"){
    val code = 
      """{
        |  val f = { (x: Int) => 1 }
        |  true && false
        |}""".stripMargin

    val node = 
      Values.BlockValue(
        Array(Values.ValDef(1, List(), Values.FuncValue(Array((4,SFunc(IndexedSeq(SInt), SBoolean, List()))), Values.FalseLeaf))),
        BinAnd(Values.TrueLeaf, Values.FalseLeaf)
      )
    val ergoTree = Values.ErgoTree.withoutSegregation(node.toSigmaProp)
    ergoTree.root match {
      case Left(value) => ???
      case Right(value) => PrettyPrintErgoTree.prettyPrint(value) shouldBe 
        """{sigmaProp({
          |  val func1 = { (func4: Int => Boolean) => false }
          |  true && false
          |})}""".stripMargin
    }
  }

  ignore("ergotree from bytes - constants usage"){
    val bytes = ErgoAlgos.decodeUnsafe("0008cd03c828caa4185b1d8efededfc5b766d19151a703c0e99223747c3487ea445c3a74")
    val ergoTree = DefaultSerializer.deserializeErgoTree(bytes)
    ergoTree.root match {
      case Left(value) => ???
      case Right(value) =>
        PrettyPrintErgoTree.prettyPrint(value) shouldBe "{SigmaProp(ProveDlog(ECPoint(c828ca,d655c0,...)))}"
        // Can ergotree above be produced by ergoscript?
        val compiledTree = compile("{SigmaProp(ProveDlog(GroupElement(c828ca,d655c0)))}")
        compiledTree shouldBe value
    }
  }
}