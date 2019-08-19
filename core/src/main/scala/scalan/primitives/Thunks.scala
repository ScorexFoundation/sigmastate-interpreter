package scalan.primitives

import scalan.compilation.{GraphVizConfig, GraphVizExport}
import scalan.{Liftable => _, _}
import debox.{Set => DSet, Buffer => DBuffer}
import spire.syntax.all.cfor

import scala.reflect.runtime.universe._
import scalan.util.{Covariant, GraphUtil}

trait Thunks extends Functions with GraphVizExport { self: Scalan =>

  type Th[+T] = Ref[Thunk[T]]
  trait Thunk[+A] { def value: A }
  class ThunkCompanion {
    def apply[T](block: => Ref[T]) = thunk_create(block)
    def forced[T](block: => Ref[T]) = thunk_create(block).force
  }
  val Thunk: ThunkCompanion = new ThunkCompanion

  implicit class RepThunkOps[T](t: Th[T]) {
    def force() = thunk_force(t)
    def map[R](f: Ref[T => R]): Th[R] = thunk_map(t, f)
    def map[R](f: Ref[T] => Ref[R]): Th[R] = thunk_map1(t, f)
  }

  implicit val thunkCont: Cont[Thunk] = new Cont[Thunk] {
    def lift[T](implicit eT: Elem[T]) = element[Thunk[T]]
    def unlift[T](implicit eFT: Elem[Thunk[T]]) = eFT.eItem
    def unapply[T](e: Elem[_]) = e match {
      case e: ThunkElem[_] => Some(asElem[Thunk[T]](e))
      case _ => None
    }
  }

  import Liftables._
  import scala.reflect.{ClassTag, classTag}
  type SThunk[T] = () => T

  case class ThunkConst[ST, T](constValue: SThunk[ST], lT: Liftable[ST, T])
      extends BaseDef[Thunk[T]]()(thunkElement(lT.eW))
         with LiftedConst[SThunk[ST], Thunk[T]] {
    val liftable: Liftable[SThunk[ST], Thunk[T]] = liftableThunk(lT)
  }

  case class LiftableThunk[ST, T](lT: Liftable[ST, T]) extends Liftable[SThunk[ST], Thunk[T]] {
    import RType._
    def eW: Elem[Thunk[T]] = thunkElement(lT.eW)
    def sourceType: RType[SThunk[ST]] = {
      implicit val tST = lT.sourceType
      RType[SThunk[ST]]
    }
    def lift(x: SThunk[ST]): Ref[Thunk[T]] = ThunkConst(x, lT)
    def unlift(w: Ref[Thunk[T]]): SThunk[ST] = w.node match {
      case ThunkConst(x: SThunk[_], l) if l == lT => x.asInstanceOf[SThunk[ST]]
      case _ => unliftError(w)
    }
  }

  implicit def liftableThunk[ST,T](implicit lT: Liftable[ST,T]): Liftable[SThunk[ST], Thunk[T]] =
    LiftableThunk(lT)


  case class ThunkElem[A](override val eItem: Elem[A])
    extends EntityElem1[A, Thunk[A], Thunk](eItem, container[Thunk]) {
    override lazy val liftable = asLiftable[SThunk[_], Thunk[A]](liftableThunk(eItem.liftable))
    override lazy val typeArgs = TypeArgs("A" -> (eItem -> Covariant))
  }

  implicit def thunkElement[T](implicit eItem: Elem[T]): Elem[Thunk[T]] =
    cachedElemByClass(eItem)(classOf[ThunkElem[T]])
  implicit def extendThunkElement[T](elem: Elem[Thunk[T]]): ThunkElem[T] = elem.asInstanceOf[ThunkElem[T]]

  class ThunkDef[A](val root: Ref[A], _scheduleIds: =>ScheduleIds)
    extends AstGraph with Def[Thunk[A]] {

    implicit def eA: Elem[A] = root.elem
    private var _selfType: Elem[Thunk[A]] = _
    def resultType: Elem[Thunk[A]] =
      if (_selfType != null) _selfType
      else {
        val res = thunkElement(eA)
        if (!root.isPlaceholder) _selfType = res  // memoize once root is assigned
        res
      }

    override lazy val scheduleIds: ScheduleIds = _scheduleIds

    /** NOTE on structural equality implementation
      * Every Def is assigned fresh nodeId in the constructor. As result this ThunkDef
      * instance will have unique nodeId. Thus, different ThunkDef instances will have
      * different nodeIds and hence they are NOT equal.
      * */
    override lazy val hashCode: Int = _nodeId //41 * (41 + root.hashCode) + schedule.hashCode
    def canEqual(other: Any) = other.isInstanceOf[ThunkDef[_]]
    override def equals(other: Any) =
      other match {
        case that: ThunkDef[_] => _nodeId == that._nodeId
        case _ => false
      }
    override def toString = s"Th($root, [${scheduleIds.toArray.mkString(",")}])"


    // Product implementation
    def productElement(n: Int): Any = n match {
      case 0 => root
      case _ => throw new NoSuchElementException(s"ThunkDef.productElement($n) is undefined")
    }
    def productArity: Int = 1

    override def boundVars = Nil
    override lazy val freeVars = if (schedule.isEmpty) Array(root) else super.freeVars

    override protected def getDeps: Array[Sym] = freeVars.toArray

    val roots = Array(root)
    override lazy val rootIds: DBuffer[Int] = super.rootIds
    override def isIdentity: Boolean = false
  }
  object ThunkDef {
    def unapply(d: ThunkDef[_]): Option[(Ref[T], Schedule) forSome {type T}] = d match {
      case th: ThunkDef[_] => Some((th.root, th.schedule))
      case _ => None
    }
  }

  class ThunkScope(val parent: ThunkScope, val thunkSym: Ref[Any]) {
    private val bodyIds: DSet[Int] = DSet.ofSize(16)
    private val bodyDefs: AVHashMap[Def[_], Def[_]] = AVHashMap(32)

    @inline final def isEmptyBody: Boolean = bodyIds.isEmpty

    def +=(sym: Sym): Unit = {
      val d = sym.node
      bodyIds += d.nodeId
      bodyDefs.put(d, d)
    }

    def scheduleForResult(root: Ref[Any]): DBuffer[Int] = {
      val sch = GraphUtil.depthFirstOrderFrom(
        DBuffer(root.node.nodeId),
        { id: Int =>
          val deps = getSym(id).node.deps
          val res = DBuffer.ofSize[Int](deps.length)
          cfor(0)(_ < deps.length, _ + 1) { i =>
            val s = deps(i)
            val id = s.node.nodeId
            if (bodyIds(id) && !s.isVar)
              res += id
          }
          res
        }
      )
      sch
    }

    def findDef[T](d: Def[T]): Ref[T] = {
      val existingOpt = bodyDefs.get(d)
      if (existingOpt.isDefined) return existingOpt.get.self.asInstanceOf[Ref[T]]
      if (parent == null)
        findGlobalDefinition(d)
      else
        parent.findDef(d)
    }
  }

  class ThunkStack {
    var stack = List[ThunkScope]()
    @inline def top: Nullable[ThunkScope] = if (stack.isEmpty) Nullable.None else Nullable(stack.head)
    def push(e: ThunkScope): this.type = { stack = e :: stack; this }
    @inline def pop: ThunkScope = {
      val res = stack.head
      stack = stack.tail
      res
    }
    def beginScope(thunkSym: Ref[Any]): ThunkScope = {
      val parent = if (stack.isEmpty) null else stack.head
      val scope = new ThunkScope(parent, thunkSym)
      this.push(scope)
      scope
    }
    @inline def endScope(): Unit = { this.pop }
  }
  protected val thunkStack = new ThunkStack

  implicit def repToThunk[A](block: Ref[A]): Ref[Thunk[A]] = thunk_create(block)

  def thunk_create[A](block: => Ref[A]): Ref[Thunk[A]] = {
    var scheduleIds: ScheduleIds = null
    val resPH = placeholder(Lazy(AnyElement)).asInstanceOf[Ref[A]] // will be known after block is evaluated
    val newThunk = new ThunkDef(resPH, { assert(scheduleIds != null); scheduleIds })
    val newThunkSym = newThunk.self

    val newScope = thunkStack.beginScope(newThunkSym)
    // execute block and add all new definitions to the top scope (see createDefinition)
    // reify all the effects during block execution
    val res = block
    resPH.assignDefFrom(res)
    scheduleIds =
      if (res.isVar) DBuffer.ofSize(0)
      else if (newScope.isEmptyBody) DBuffer.ofSize(0)
      else newScope.scheduleForResult(res)

    val sh = newThunk.scheduleIds  // force lazy value in newThunk (see ThunkDef._scheduleIds argument above)
    thunkStack.endScope()
    toExp(newThunk, newThunkSym)
  }

  def thunk_map[A, B](t: Th[A], f: Ref[A => B]): Th[B] = {
    Thunk {
      f(thunk_force(t))
    }
  }
  def thunk_map1[A, B](t: Th[A], f: Ref[A] => Ref[B]): Th[B] = {
    Thunk {
      f(thunk_force(t))
    }
  }

  var isInlineThunksOnForce = false

  def forceThunkByMirror[A](thunk: Th[A], subst: MapTransformer = MapTransformer.empty()): Ref[A] = {
    val th = thunk.node.asInstanceOf[ThunkDef[A]]
    forceThunkDefByMirror(th, subst)
  }
  def forceThunkDefByMirror[A](th: ThunkDef[A], subst: MapTransformer = MapTransformer.empty()): Ref[A] = {
    val body = th.scheduleIds
    val t = DefaultMirror.mirrorSymbols(subst, NoRewriting, th, body)
    t(th.root)
  }

  def thunk_force[A](t: Th[A]): Ref[A] =
    if (isInlineThunksOnForce)
      t.node match {
        case th @ ThunkDef(_, _) =>
          forceThunkByMirror(t)
        case _ => ThunkForce(t)
      }
    else
      ThunkForce(t)

  case class ThunkForce[A](thunk: Ref[Thunk[A]]) extends Def[A] {
    implicit def resultType = thunk.elem.eItem
    override def transform(t: Transformer) = ThunkForce(t(thunk))
  }

  override protected def matchDefs(d1: Def[_], d2: Def[_], allowInexactMatch: Boolean, subst: Subst): Nullable[Subst] = d1 match {
    case ThunkDef(root1, sch1) => d2 match {
      case ThunkDef(root2, sch2) =>
        var res = matchIterators(sch1.iterator, sch2.iterator, allowInexactMatch, subst)
        if (res.isDefined)
          res = matchExps(root1, root2, allowInexactMatch, res.get)
        res
      case _ => Nullable.None
    }
    case _ =>
      super.matchDefs(d1, d2, allowInexactMatch, subst)
  }

  object ConstantThunk {
    def unapply(d: Def[_]): Option[Const[_]] = d match {
      case ThunkDef(root @ Def(c @Const(_)), Seq(s1)) if root == s1 => Some(c)
      case _ => None
    }
    def unapply(s: Sym): Option[Const[_]] = unapply(s.node)
  }


  override protected def formatDef(d: Def[_])(implicit config: GraphVizConfig): String = d match {
    case ThunkDef(r, sch) => s"Thunk($r, [${sch.mkString(",")}])"
    case _ => super.formatDef(d)
  }

  override protected def nodeColor(td: TypeDesc, d: Def[_])(implicit config: GraphVizConfig) = td match {
    case _: ThunkElem[_] => "red"
    case _ => super.nodeColor(td, d)
  }
}

