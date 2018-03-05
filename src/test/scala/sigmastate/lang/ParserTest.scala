package sigmastate.lang

import org.scalatest.{PropSpec, Matchers}
import org.scalatest.prop.PropertyChecks
import scorex.crypto.encode.Base58
import sigmastate._
import sigmastate.lang.Terms._
import sigmastate.utxo.SizeOf

class ParserTest extends PropSpec with PropertyChecks with Matchers {

  def parse(x: String): UValue = {
    try {
      val res = Parser(x).get.value
//      Parser.logged.foreach(println)
      res
    } catch {
      case e: Exception =>
        Parser.logged.foreach(println)
        throw e
    }
  }

  property("simple expressions") {
    parse("10") shouldBe IntConstant(10)
    parse("10+11") shouldBe Plus(IntConstant(10), IntConstant(11))
    parse("(10+11)") shouldBe Plus(IntConstant(10), IntConstant(11))
    parse("(10+11) + 12") shouldBe Plus(Plus(IntConstant(10), IntConstant(11)), IntConstant(12))
    parse("10   + 11 + 12") shouldBe Plus(Plus(IntConstant(10), IntConstant(11)), IntConstant(12))
    parse("1+2+3+4+5") shouldBe Plus(Plus(Plus(Plus(IntConstant(1), IntConstant(2)), IntConstant(3)), IntConstant(4)), IntConstant(5))
    parse("1==1") shouldBe EQ(IntConstant(1), IntConstant(1))
    parse("true && true") shouldBe AND(TrueLeaf, TrueLeaf)
    parse("true || false") shouldBe OR(TrueLeaf, FalseLeaf)
    parse("true || (true && false)") shouldBe OR(TrueLeaf, AND(TrueLeaf, FalseLeaf))
    parse("false || false || false") shouldBe OR(OR(FalseLeaf, FalseLeaf), FalseLeaf)
    parse("(1>= 0)||(3 >2)") shouldBe OR(GE(IntConstant(1), IntConstant(0)), GT(IntConstant(3), IntConstant(2)))
  }

  property("priority in binary expressions") {
    parse("1 == 0 || 3 == 2") shouldBe OR(EQ(IntConstant(1), IntConstant(0)), EQ(IntConstant(3), IntConstant(2)))
    parse("3 + 2 > 2 + 1") shouldBe GT(Plus(IntConstant(3), IntConstant(2)), Plus(IntConstant(2), IntConstant(1)))
    parse("1 >= 0 || 3 > 2") shouldBe OR(GE(IntConstant(1), IntConstant(0)), GT(IntConstant(3), IntConstant(2)))
  }

  property("comma operators") {
    parse("1, 2") shouldBe Comma(IntConstant(1), IntConstant(2))
    parse("1, 2, 3") shouldBe Comma(Comma(IntConstant(1), IntConstant(2)), IntConstant(3))
    parse("1, 2 + 3") shouldBe Comma(IntConstant(1), Plus(IntConstant(2), IntConstant(3)))
  }

  property("tuple constructor") {
    parse("()") shouldBe UnitConstant
    parse("(1)") shouldBe IntConstant(1)
    parse("(1, 2)") shouldBe Tuple(IntConstant(1), IntConstant(2))
    parse("(1, X + 1)") shouldBe Tuple(IntConstant(1), Plus(Ident("X").asValue[SInt.type], IntConstant(1)))
    parse("(1, 2, 3)") shouldBe Tuple(IntConstant(1), IntConstant(2), IntConstant(3))
    parse("(1, 2 + 3, 4)") shouldBe Tuple(IntConstant(1), Plus(IntConstant(2), IntConstant(3)), IntConstant(4))
  }

  property("let/ref constructs") {
    parse(
      """let X = 10;
        |3 > 2
      """.stripMargin) shouldBe Block(Some(Let("X", IntConstant(10))), GT(IntConstant(3), IntConstant(2)))

    parse("{let X = 10; 3 > 2}") shouldBe Block(Some(Let("X", IntConstant(10))), GT(IntConstant(3), IntConstant(2)))
    parse("{let X = 3 + 2; 3 > 2}") shouldBe Block(Some(Let("X", Plus(IntConstant(3), IntConstant(2)))), GT(IntConstant(3), IntConstant(2)))
    parse("{let X = if(true) then true else false; false}") shouldBe Block(Some(Let("X", If(TrueLeaf, TrueLeaf, FalseLeaf))), FalseLeaf)

    val expr = parse(
      """let X = 10;
        |let Y = 11;
        |X > Y
      """.stripMargin)

    expr shouldBe Block(Some(Let("X", IntConstant(10))), Block(Some(Let("Y", IntConstant(11))), typed[SInt.type, SInt.type](Ident("X"), Ident("Y"))(GT)))
  }

  property("types") {
    parse("let X: Int = 10; 3 > 2") shouldBe Block(Some(Let("X", IntConstant(10))), GT(IntConstant(3), IntConstant(2)))
    parse("""let X: (Int, Boolean) = (10, true); 3 > 2""") shouldBe Block(Some(Let("X", Tuple(IntConstant(10), TrueLeaf))), GT(IntConstant(3), IntConstant(2)))
    parse("""let X: Array[Int] = [1,2,3]; X.size""") shouldBe
      Block(Some(Let("X", ConcreteCollection(IndexedSeq(IntConstant(1), IntConstant(2), IntConstant(3))))),
            Select(Ident("X", NoType), "size"))
    parse("""let X: (Array[Int], Box) = ([1,2,3], INPUT); X._1""") shouldBe
        Block(Some(Let("X", Tuple(ConcreteCollection(IndexedSeq(IntConstant(1), IntConstant(2), IntConstant(3))), Ident("INPUT")))),
          Select(Ident("X", NoType), "_1"))
  }

  property("multiline") {
    parse(
      """
        |
        |false
        |
        |
      """.stripMargin) shouldBe FalseLeaf

    parse(
      """let X = 10;
        |
        |true
      """.stripMargin) shouldBe Block(Some(Let("X", IntConstant(10))), TrueLeaf)
    parse(
      """let X = 11;
        |true
      """.stripMargin) shouldBe Block(Some(Let("X", IntConstant(11))), TrueLeaf)

    parse(
      """
        |
        |let X = 12;
        |
        |3
        | +
        |  2
        |
      """.stripMargin) shouldBe Block(Some(Let("X", IntConstant(12))), Plus(IntConstant(3), IntConstant(2)))
  }

  property("if") {
    parse("if(true) then 1 else 2") shouldBe If(TrueLeaf, IntConstant(1), IntConstant(2))
    parse("if(true) then 1 else if(X==Y) then 2 else 3") shouldBe If(TrueLeaf, IntConstant(1), If(EQ(Ident("X"), Ident("Y")), IntConstant(2), IntConstant(3)))
    parse(
      """if ( true )
        |then 1
        |else if(X== Y)
        |     then 2
        |       else 3""".stripMargin) shouldBe If(TrueLeaf, IntConstant(1), If(EQ(Ident("X"), Ident("Y")), IntConstant(2), IntConstant(3)))

    parse("if (true) then false else false==false") shouldBe If(TrueLeaf, FalseLeaf, EQ(FalseLeaf, FalseLeaf))

    parse(
      """if

             (true)
        |then { let A = 10;
        |  1 }
        |else if ( X == Y) then 2 else 3""".stripMargin) shouldBe If(
      TrueLeaf,
      Block(Some(Let("A", Block(None, IntConstant(10)))), IntConstant(1)),
      If(EQ(Ident("X"), Ident("Y")), IntConstant(2), IntConstant(3))
    )

  }

  property("arrays") {
    parse("[]") shouldBe(ConcreteCollection(IndexedSeq.empty)(NoType))
    parse("[1]") shouldBe(ConcreteCollection(IndexedSeq(IntConstant(1)))(SInt))
    parse("[1, X]") shouldBe(ConcreteCollection(IndexedSeq(IntConstant(1), Ident("X")))(SInt))
    parse("[1, X + 1, []]") shouldBe(ConcreteCollection(
      IndexedSeq(
        IntConstant(1),
        Plus(Ident("X").asValue[SInt.type], IntConstant(1)),
        ConcreteCollection(IndexedSeq.empty)(NoType)))(SInt))
    parse("[[X + 1]]") shouldBe ConcreteCollection[SCollection[SInt.type]](
      IndexedSeq(ConcreteCollection[SInt.type](IndexedSeq(
                  Plus(Ident("X").asValue[SInt.type], IntConstant(1))))))
  }

  property("global functions") {
    parse("f(x)") shouldBe Apply(Ident("f"), IndexedSeq(Ident("x")))
    parse("f((x, y))") shouldBe Apply(Ident("f"), IndexedSeq(Tuple(IndexedSeq(Ident("x"), Ident("y")))))
    parse("f(x, y)") shouldBe Apply(Ident("f"), IndexedSeq(Ident("x"), Ident("y")))
  }

  property("lambdas") {
    parse("fun (x: Int) = x + 1") shouldBe Lambda(IndexedSeq("x" -> SInt), Plus(Ident("x").asValue[SInt.type], IntConstant(1)))
  }

  property("unary operations") {
    parse("!x") shouldBe Not(Ident("x").asValue[SBoolean.type])
    parse("!!x") shouldBe Not(Not(Ident("x").asValue[SBoolean.type]))
    parse("!x && y") shouldBe AND(Not(Ident("x").asValue[SBoolean.type]), Ident("y").asValue[SBoolean.type])
    parse("!x && !y") shouldBe AND(Not(Ident("x").asValue[SBoolean.type]), Not(Ident("y").asValue[SBoolean.type]))
    parse("#x") shouldBe SizeOf(Ident("x").asValue[SCollection[NoType.type]])
    parse("#x + 1") shouldBe Plus(SizeOf(Ident("x").asValue[SCollection[NoType.type]]), IntConstant(1))
  }

  property("get field of ref") {
    parse("XXX.YYY") shouldBe Select(Ident("XXX"), "YYY")
    parse("""
        |
        | X.Y
        |
      """.stripMargin) shouldBe Select(Ident("X"), "Y")
  }

//  property("multisig sample") {
//    val script =
//      """
//        |
//        |let A = base58'PK1PK1PK1PK1PK1'
//        |let B = base58'PK2PK2PK2PK2PK2'
//        |let C = base58'PK3PK3PK3PK3PK3'
//        |
//        |let W = TX.BODYBYTES
//        |let P = TX.PROOF
//        |let V = checkSig(W,P,A)
//        |
//        |let AC = if(V) then 1 else 0
//        |let BC = if(checkSig(TX.BODYBYTES,TX.PROOF,B)) then 1 else 0
//        |let CC = if(checkSig(TX.BODYBYTES,TX.PROOF,C)) then 1 else 0
//        |
//        | AC + BC+ CC >= 2
//        |
//      """.stripMargin
//    parse(script)
//  }
//
//  property("isDefined/get") {
//    parse("isDefined(X)") shouldBe IS_DEFINED(REF("X"))
//    parse("if(isDefined(X)) then get(X) else Y") shouldBe IF(IS_DEFINED(REF("X")), GET(REF("X")), REF("Y"))
//  }
//
//  property("EVALUATE patmat") {
//    Evaluator.apply(
//      Context.empty,
//      parse(
//        """
//          |let MULTICHARVARNAME = Some(500)
//          |
//          |let Z = match(MULTICHARVARNAME) {
//          | case None => 8
//          | case Some(B) => B + B
//          | }
//          |
//          | get(Some(Z)) + 1
//          |
//      """.stripMargin)
//    ) shouldBe Right(1001)
//
//    Evaluator.apply(
//      Context.empty,
//      parse(
//        """
//          |
//          |let X = Some(10)
//          |
//          |match(X) {
//          |  case None => 0
//          |  case Some(V) => V + V + V + V
//          |}
//        """.stripMargin)
//    ) shouldBe Right(40)
//
//    Evaluator.apply(
//      Context.empty,
//      parse(
//        """
//          |
//          |let X = Some(10)
//          |
//          |match(X) {
//          |  case Some(V) => V + V + V + V
//          |  case None => 0
//          |}
//        """.stripMargin)
//    ) shouldBe Right(40)
//  }

  //  property("bytestr expressions") {
  //    parse("checkSig(base58'333', base58'222', base58'111')") shouldBe SIG_VERIFY(
  //      CONST_BYTEVECTOR(ByteVector(Base58.decode("333").get)),
  //      CONST_BYTEVECTOR(ByteVector(Base58.decode("222").get)),
  //      CONST_BYTEVECTOR(ByteVector(Base58.decode("111").get))
  //    )
  //
  //    parse("false || checkSig(base58'333', base58'222', base58'111')") shouldBe OR(
  //      FalseLeaf,
  //      SIG_VERIFY(
  //        CONST_BYTEVECTOR(ByteVector(Base58.decode("333").get)),
  //        CONST_BYTEVECTOR(ByteVector(Base58.decode("222").get)),
  //        CONST_BYTEVECTOR(ByteVector(Base58.decode("111").get))
  //      )
  //    )
  //  }
  //
}
