package sigmastate.lang

import fastparse.core.Logger
import fastparse.{WhitespaceApi, core}
import sigmastate._
import sigmastate.lang.Terms._
import scorex.crypto.encode.Base58

import scala.collection.mutable

class ParserException(msg: String) extends Exception(msg)

object Parser {

  val logged = mutable.Buffer.empty[String]
  implicit val logger = Logger(logged.append(_))

  private val Base58Chars = "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz"

  private val FirstCharInVarField = "QWERTYUIOPASDFGHJKLZXCVBNM"
  private val OtherCharInVarField = FirstCharInVarField + "1234567890[]"

  private val White = WhitespaceApi.Wrapper {
    import fastparse.all._
    NoTrace(CharIn(" ", "\t", "\r", "\n").rep)
  }
  private val keywords = Set("if", "then", "else", "let")
  import fastparse.noApi._
  import White._

  val alpha = CharIn('A' to 'Z') | CharIn('a' to 'z')
  val digit = CharIn('0' to '9')
  val varName = (alpha ~ ((alpha | digit).rep())).!.filter(!keywords(_))

  private def numberP: P[IntConstant]      = digit.rep(min = 1).!.map(t => IntConstant(t.toLong))
  private def trueP: P[TrueLeaf.type]      = P("true").map(_ => TrueLeaf)
  private def falseP: P[FalseLeaf.type]    = P("false").map(_ => FalseLeaf)
  private def unitP: P[UnitConstant.type]  = P("()").map(_ => UnitConstant)
  private def ident: P[Ident]               = P(varName).map(x => Ident(x.trim))
  private def byteVectorP: P[ByteArrayConstant] =
    P("base58'" ~ CharsWhileIn(Base58Chars).! ~ "'")
        .map(x => ByteArrayConstant(Base58.decode(x).get))

  private def commaList: P[List[Value[SType]]] = (expr ~ ("," ~ expr).rep()).map {
    case (h, t) => h :: t.toList
  }

  private def bracketsP: P[Value[SType]] = P("[" ~ commaList ~ "]").map {
    case Nil => ConcreteCollection(IndexedSeq.empty)(NoType)
    case h :: t => ConcreteCollection(h +: t.toIndexedSeq)(h.tpe)
  }
  private def bracesP: P[Value[SType]] = P("(" ~ commaList ~ ")").map {
    case Nil => UnitConstant
    case h :: Nil => h
    case h :: t => Tuple(h +: t.toIndexedSeq)
  }
  private def curlyBracesP: P[Value[SType]]    = P("{" ~ block ~ "}")

  private def statement = P(letP).log()
  private def letP: P[Let]             = P("let " ~ varName ~ "=" ~ expr ~ ";").map { case ((x, y)) => Let(x, y) }

  private def func: P[Value[SType]]         = P(ident)

  private def applyP: P[Apply]         = P(func ~ "(" ~ commaList ~ ")").map {
    case (f, Nil) => Apply(f, IndexedSeq.empty)
    case (f, items) => Apply(f, items.toIndexedSeq)
  }

  private def ifP: P[If[SType]]        = P("if" ~ "(" ~ expr ~ ")" ~ "then" ~ expr ~ "else" ~ expr)
    .map { case (x, y, z) => If(x.asValue[SBoolean.type], y, z) }
    
  private def block: P[Value[SType]] = P("\n".rep ~ statement.rep ~ expr).map {
    case ((Nil, y)) => y
    case ((all, y)) => all.foldRight(y) { case (r, curr) => Block(Some(r), curr) }
  }

  private def expr = P(applyP | binaryOp(priority) | bracesP | atom).log()

  private def atom: P[Value[_ <: SType]] =
    P(ifP | byteVectorP | numberP | trueP | falseP | unitP  | curlyBracesP | bracketsP | ident )

  def apply(str: String): core.Parsed[Value[_ <: SType], Char, String] = (block ~ End).log().parse(str)

  private val priority = List("||", "&&", "==", ">=", ">", "+", "-", "*", ".")

  private def binaryOp(rest: List[String]): P[Value[SType]] = rest match {
    case Nil => atom
    case lessPriorityOp :: restOps =>
      val operand = binaryOp(restOps)
      var p = P(operand ~ (lessPriorityOp.! ~ operand).rep())
      if (lessPriorityOp == "+") p = p.log()
      p.map {
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
                case "."  => y match {
                  case Ident(fieldName, t) => Select(r2, fieldName)
                  case _ => error(s"Invalid field name $y")
                }
              }
          }

      }
  }

  def error(msg: String) = throw new ParserException(msg)
}
