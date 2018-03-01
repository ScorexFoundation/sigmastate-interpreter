package com.wavesplatform.lang

import fastparse.{WhitespaceApi, core}
import sigmastate._
import sigmastate.lang.Terms._
import scorex.crypto.encode.Base58

object Parser {

  private val Base58Chars = "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz"

  private val FirstCharInVarField = "QWERTYUIOPASDFGHJKLZXCVBNM"
  private val OtherCharInVarField = FirstCharInVarField + "1234567890[]"

  private val White = WhitespaceApi.Wrapper {
    import fastparse.all._
    NoTrace(CharIn(" ", "\t", "\r", "\n").rep)
  }

  import fastparse.noApi._
  import White._

  val alpha = CharIn('A' to 'Z') | CharIn('a' to 'z')
  val digit = CharIn('0' to '9')
  val varName = (alpha ~ ((alpha | digit).rep())).!

  private def numberP: P[IntConstant]      = P(digit.rep(min = 1).!.map(t => IntConstant(t.toLong)))
  private def trueP: P[TrueLeaf.type]      = P("true").map(_ => TrueLeaf)
  private def falseP: P[FalseLeaf.type]    = P("false").map(_ => FalseLeaf)
  private def byteVectorP: P[ByteArrayConstant] =
    P("base58'" ~ CharsWhileIn(Base58Chars).! ~ "'")
        .map(x => ByteArrayConstant(Base58.decode(x).get))

  private def bracketsP: P[Value[SType]] = P("[" ~ ((expr ~ ("," ~ expr).rep()?) ~ "]")).map {
    case Some((h, t)) => ConcreteCollection(h +: t.toIndexedSeq)(h.tpe)
    case None => ConcreteCollection(IndexedSeq.empty)(NoType)
  }

  private def bracesP: P[Value[SType]] = P("(" ~ ((expr ~ ("," ~ expr).rep()?) ~ ")")).map {
    case None => UnitConstant
    case Some((h, t)) if t.isEmpty => h
    case Some((h, t)) => Tuple(h +: t.toIndexedSeq)
  }

  private def curlyBracesP: P[Value[SType]]    = P("{" ~ block ~ "}")
  private def letP: P[LET]             = P("let " ~ varName ~ "=" ~ block).map { case ((x, y)) => LET(x, y) }
  private def refP: P[REF]             = P(varName).map(x => REF(x.trim))
//  private def applyP: P[Apply]         = P(expr ~ bracesP).map {
//    case (f, UnitConstant) => Apply(f, IndexedSeq.empty)
//    case (f, Tuple(items)) => Apply(f, items)
//    case (f, arg)          => Apply(f, IndexedSeq(arg))
//  }

  private def ifP: P[If[SType]]        = P("if" ~ "(" ~ block ~ ")" ~ "then" ~ block ~ "else" ~ block)
    .map { case (x, y, z) => If(x.asValue[SBoolean.type], y, z) }
    
  private def getterP: P[GETTER] = P(refP ~ "." ~ varName).map { case ((b, f)) => GETTER(b, f.trim) }
  private def block: P[Value[SType]] = P("\n".rep ~ letP.rep ~ expr ~ ";".rep).map {
    case ((Nil, y)) => y
    case ((all, y)) => all.foldRight(y) { case (r, curr) => Block(Some(r), curr) }
  }

  private def expr = P(binaryOp(priority) | atom)

  private def atom: P[Value[_ <: SType]] =
    P(ifP | byteVectorP | numberP | trueP | falseP | bracesP | curlyBracesP | bracketsP | getterP | refP /*| applyP*/)

  def apply(str: String): core.Parsed[Value[_ <: SType], Char, String] = block.parse(str)

  private val priority = List("||", "&&", "==", ">=", ">", "+", "-", "*")

  private def binaryOp(rest: List[String]): P[Value[SType]] = rest match {
    case Nil => atom
    case lessPriorityOp :: restOps =>
      val operand = binaryOp(restOps)
      P(operand ~ (lessPriorityOp.! ~ operand).rep()).map {
        case ((left: Value[SType], r: Seq[(String, Value[SType])])) =>
          r.foldLeft(left) {
            case (r2, (op, y)) =>
              op match {
                case "||" => typed[SBoolean.type, SBoolean.type](r2, y)(OR.apply)
                case "&&" => typed[SBoolean.type, SBoolean.type](r2, y)(AND.apply)
                case "==" => EQ(r2, y)
                case ">=" => typed[SInt.type, SInt.type](r2, y)(GE)
                case ">"  => typed[SInt.type, SInt.type](r2, y)(GT)
                case "+"  => typed[SInt.type, SInt.type](r2, y)(Plus)
                case "-"  => typed[SInt.type, SInt.type](r2, y)(Minus)
              }
          }

      }
  }
}
