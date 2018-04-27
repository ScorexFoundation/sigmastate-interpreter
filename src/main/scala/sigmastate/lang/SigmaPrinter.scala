package sigmastate.lang

import sigmastate._
import sigmastate.Values._
import sigmastate.lang.Terms._

/** Sigma pretty printing. */
class SigmaPrinter extends org.bitbucket.inkytonik.kiama.output.PrettyPrinter {

  import org.bitbucket.inkytonik.kiama.output.PrettyPrinterTypes.Document

  /** Format a lambda expression. */
  def format(t : SValue) : Document =
    pretty(toDoc(t))

  /** The layout from formatting a lambda expression. */
  def formattedLayout(t : SValue) : String =
    format(t).layout

  /** Format a type. */
  def format(t : SType) : Document =
    pretty(typeToDoc(t))

  /** The layout from formatting a type. */
  def formattedLayout(t : SType) : String =
    format(t).layout

  /**
    * Convert an expression node to a pretty-printing document in
    * fully-parenthesised style.
    */
  def toDoc(t : SValue) : Doc =
    t match {
      case IntConstant(d) => value(d)
      case Ident(i,_) => i
      case Lambda(args, tLam, Some(e)) =>
        parens('\\' <> parens(lsep(args.map { case (n, targ) => n <+> ": " <+> typedeclToDoc(targ) }.to, comma)) <>
          typedeclToDoc(tLam) <+> '.' <+>
          group(nest(toDoc(e))))
      case Apply(e, args)        => parens(toDoc(e) <+> parens(lsep(args.map(toDoc).to, comma)))

//      case Opn(l, AddOp(), r) => binToDoc(l, "+", r)
//      case Opn(l, SubOp(), r) => binToDoc(l, "-", r)

//      case Let(i, t, e1, e2) =>
//        parens("let" <+> i <> typedeclToDoc(t) <+> '=' <>
//            nest(line <> toDoc(e1)) <+> "in" <>
//            nest(line <> toDoc(e2)))
//      case Letp(bs, e) =>
//        parens("letp" <>
//            nest(line <> vsep(bs.map(b => b.i <+> '=' <+> toDoc(b.e)))) <+>
//            "in" <>
//            nest(line <> toDoc(e)))
    }

  /**
    * Return a pretty-printing document for an instance of a type declaration.
    */
  def typedeclToDoc(t : SType) : Doc =
    if (t == NoType)
      emptyDoc
    else
      space <> ':' <+> typeToDoc(t)

  /**
    * Return a pretty-printing document for an instance of a type.
    */
  def typeToDoc(t : SType) : Doc =
    t match {
      case SInt       => "Int"
      case SFunc(dom, t2, _) => parens(lsep(dom.map(typeToDoc).to, comma)) <+> "->" <+> typeToDoc(t2)
      case NoType        => "NoType" // Not used
      case _ => s"<unknown type $t>"
    }

  /**
    * Return a pretty-printing document for an instance of a binary expression.
    */
  def binToDoc(l : SValue, op : String, r : SValue) : Doc =
    parens(toDoc(l) <+> op <+> toDoc(r))

}

/**
  * Lambda calculus pretty printing.
  */
object SigmaPrinter extends SigmaPrinter
