package playground

import org.bitbucket.inkytonik.kiama.rewriting.Rewriter.{everywherebu, rule}
/*
object FreeDemo extends App{

  sealed trait Value {
    def evaluated: Boolean
  }


  sealed trait EvaluatedValue[V <: Value] extends Value {
    override lazy val evaluated = true
  }
  sealed trait NotReadyValue[V <: Value] extends Value {
    override lazy val evaluated = false
  }


  sealed trait IntLeaf extends Value
  case class IntLeafConstant(i: Long) extends IntLeaf with EvaluatedValue[IntLeaf]
  trait NotReadyValueIntLeaf extends IntLeaf with NotReadyValue[IntLeaf]
  case object Random extends NotReadyValueIntLeaf
  case object NotReady extends NotReadyValueIntLeaf


  sealed trait CollLeaf[V <: Value] extends Value
  case class ConcreteColl[V <: Value](value: Seq[V]) extends CollLeaf[V] with EvaluatedValue[CollLeaf[V]]{
    lazy val isLazy = value.exists(_.isInstanceOf[NotReadyValue[_]] == true)
    override lazy val evaluated = !isLazy
  }
  trait LazyColl [V <: Value] extends CollLeaf[V] with NotReadyValue[LazyColl[V]]
  case object RandomColl extends LazyColl[IntLeaf]


  class Map[IV <: Value, OV <: Value](input: CollLeaf[IV], val mapper: IV => OV) extends LazyColl[OV]

  case class Inc(input: CollLeaf[IntLeaf]) extends Map(input, (il => il match {
    case lz: NotReadyValueIntLeaf => lz
    case IntLeafConstant(i) => IntLeafConstant(i+1)
  }): IntLeaf => IntLeaf)

  class Fold[IV <: Value](val input: CollLeaf[IV], val folder: (IV, IV) => IV, val zero: IV) extends NotReadyValue[IV]

  case class Sum(override val input: CollLeaf[IntLeaf]) extends Fold[IntLeaf](input, (
    {case (s, i) =>
      (s, i) match {
        case (si: IntLeafConstant, ii: IntLeafConstant) => IntLeafConstant(si.i + ii.i)
        case _ => NotReady
      }
  }: (IntLeaf, IntLeaf) => IntLeaf), zero = IntLeafConstant(0)) with IntLeaf


  case class Plus(l: IntLeaf, r: IntLeaf) extends NotReadyValueIntLeaf

  def reduce = everywherebu(rule[Value]{
    case Random => IntLeafConstant(scala.util.Random.nextInt(100))
    case RandomColl => ConcreteColl(Seq(IntLeafConstant(5), Random))
    case Plus(l: IntLeafConstant, r: IntLeafConstant) => IntLeafConstant(l.i + r.i)
    case i: Inc => i.input match {
      case c@ConcreteColl(v) if !c.isLazy  => ConcreteColl(v.map(i.mapper))
      case _ => i
    }
    case f: Fold[_] => f.input match {
      case c @ ConcreteColl(v) if !c.isLazy =>
        v.foldLeft(f.zero){case (s,i) => f.folder(s,i)}
      case _ => f
    }
  })

  val exp  = Plus(Plus(IntLeafConstant(5), Random), Random)
  val exp2 = Plus(Sum(Inc(RandomColl)), IntLeafConstant(5))

  println(reduce(exp2).flatMap(reduce))
}*/