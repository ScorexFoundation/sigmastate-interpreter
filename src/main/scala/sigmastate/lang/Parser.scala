package sigmastate.lang

import fastparse.core.Logger
import fastparse.{WhitespaceApi, core}
import sigmastate._
import sigmastate.lang.Terms._
import scorex.crypto.encode.Base58
import sigmastate.utxo.SizeOf

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
  private def emptyArrayP: P[Value[SType]]  = P("[]").map(_ => ConcreteCollection(IndexedSeq.empty)(NoType))
  private def ident: P[Ident]               = P(varName).map(x => Ident(x.trim))
  private def byteVectorP: P[ByteArrayConstant] =
    P("base58'" ~ CharsWhileIn(Base58Chars).! ~ "'")
        .map(x => ByteArrayConstant(Base58.decode(x).get))

  private def commaList: P[List[Value[SType]]] = (expr ~ ("," ~ expr).rep()).map {
    case (h, t) => h :: t.toList
  }

  private def curlyBracesP: P[Value[SType]]    = P("{" ~/ block ~ "}")

  private def statement = P(letP).log()
  private def letP: P[Let]             = P("let " ~/ varName ~ "=" ~/ expr ~ ";").map { case ((x, y)) => Let(x, y) }

  private def func: P[Value[SType]] = P(ident)

  private def apply: P[Value[SType]]         = P(func ~ (unitP | parens).?).log().map {
    case (f, Some(UnitConstant)) => Apply(f, IndexedSeq.empty)
    case (f, Some(Tuple(items))) => Apply(f, items)
    case (f, Some(x)) => Apply(f, flattenComma(x).toIndexedSeq)
    case (x: Ident, None) => x
  }

  private def ifP: P[If[SType]]        = P("if" ~ "(" ~ expr ~ ")" ~ "then" ~ expr ~ "else" ~ expr)
    .map { case (x, y, z) => If(x.asValue[SBoolean.type], y, z) }
    
  private def block: P[Value[SType]] = P("\n".rep ~ statement.rep ~ expr).map {
    case ((Nil, y)) => y
    case ((all, y)) => all.foldRight(y) { case (r, curr) => Block(Some(r), curr) }
  }

  private lazy val binop = binaryOp(priority)

  private val allUnaryOps = List("!", "#")
  private def unaryOp: P[String] = {
    def loop(ops: List[String]): P[String] = ops match {
      case o :: Nil => P(o).!
      case h :: t => P(h).! | loop(t)
    }
    loop(allUnaryOps)
  }

  private lazy val unop = P( unaryOp ~ factor ).map { case (op, f) => op match {
      case "!" => Not(f.asValue[SBoolean.type])
      case "#" => SizeOf(f.asValue[SCollection[SAny.type]])
    }
  }

  private val parens = P( "(" ~/ binop ~ ")" ).log().map {
    case c: Comma => Tuple(flattenComma(c).toIndexedSeq)
    case x => Tuple(x)
  }

  private val bracketsP = P("[" ~/ binop ~ "]").map(t => {
    val items = t match {
      case c: Comma => flattenComma(c).toIndexedSeq
      case _ => IndexedSeq(t)
    }
    ConcreteCollection(items)(if (items.nonEmpty) items(0).tpe else NoType)
  })

  private def factor: P[Value[_ <: SType]] =
    P(ifP | byteVectorP | numberP | trueP | falseP | unitP  | emptyArrayP
      | curlyBracesP
      | bracketsP
      | parens.map {
          case t if t.items.size == 1 => t.items(0)
          case t => t
        }
      | unop | apply)

  private def expr = P(binop).log()

  def apply(str: String): core.Parsed[Value[_ <: SType], Char, String] = (block ~ End).log().parse(str)

  private val priority = List(",", "||", "&&", "==", ">=", ">", "+", "-", "*", ".")

  private def binaryOp(rest: List[String]): P[Value[SType]] = rest match {
    case Nil => factor
    case lessPriorityOp :: restOps =>
      val operand = binaryOp(restOps)
      var binop = P( operand ~ (lessPriorityOp.!  ~  operand).rep )
      binop.map {
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
                case "," => Comma(r2, y)
              }
          }

      }
  }

  def error(msg: String) = throw new ParserException(msg)
}
