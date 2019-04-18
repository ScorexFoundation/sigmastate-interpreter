package sigmastate.utils

import sigmastate.Values.ValueCompanion
import sigmastate.{ArgInfo, SMethod}
import scalan.meta.PrintExtensions._
import scala.util.Try
import sigmastate.lang.SigmaPredef.PredefinedFunc

/** Generate as a console output all InfoObject objects.
  * Those provide stable identifiers to access metadata information.
  * This should be regenerated each time metadata is changed.
  * Example:
  * object AppendInfo extends InfoObject {
  *   private val method = SMethod.fromIds(12, 9)
  *   val thisArg = method.argInfo("this")
  *   val otherArg = method.argInfo("other")
  *   ...
  * }
  * The following consistency checks are performed:
  * 1) every func/method argument has attached ArgInfo
  * 2) method is resolvable by ids (e.g. as SMethod.fromIds(12, 9))
  * 3) func is resolvable by name (e.g. predefinedOps.funcs("sigmaProp"))
  */
object GenInfoObjects extends SpecGen {

  case class OpInfo(
    opDesc: ValueCompanion,
    description: String,
    args: Seq[ArgInfo], op: Either[PredefinedFunc, SMethod])

  def main(args: Array[String]) = {
    val table = collectOpsTable()
    val rowsWithInfo =
      for ((opCode, opDesc, optM, optF) <- table if optM.nonEmpty || optF.nonEmpty)
      yield (opDesc, optM, optF)

    val infos = for (row @ (opDesc, optM, optF) <- rowsWithInfo) yield {
      (optM, optF) match {
        case (Some(m), _) =>
          val description = m.docInfo.map(i => i.description).opt()
          val args = m.docInfo.map(i => i.args).getOrElse(Seq())
          OpInfo(opDesc, description, args, Right(m))
        case (_, Some(f)) =>
          val description = f.docInfo.description
          val args = f.docInfo.args
          OpInfo(opDesc, description, args, Left(f))
        case _ => sys.error(s"Unexpected $row")
      }
    }
    val infoStrings = infos.sortBy(_.opDesc.typeName).map { info =>
      val opName = info.opDesc.typeName

      val res = info.op match {
        case Right(m) =>
          val typeId = m.objType.typeId
          assert(m.stype.tDom.length == info.args.length,
            s"Method $m has ${m.stype.tDom} arguments, but ${info.args} descriptions attached.")
          Try{SMethod.fromIds(typeId, m.methodId)}
            .fold(t => throw new RuntimeException(s"Cannot resolve method $m using SMethod.fromIds(${typeId}, ${m.methodId})"), _ => ())
          val args = info.args.map { a =>
            s"""val ${a.name}Arg: ArgInfo = method.argInfo("${a.name}")"""
          }
          s"""
            |  object ${opName}Info extends InfoObject {
            |    private val method = SMethod.fromIds(${typeId}, ${m.methodId})
            |    ${args.rep(sep = "\n    ")}
            |    val argInfos: Seq[ArgInfo] = Seq(${info.args.rep(a => s"${a.name}Arg")})
            |  }
           """.stripMargin
        case Left(f) =>
          assert(f.declaration.args.length == info.args.length,
            s"Predefined function $f has ${f.declaration.args} arguments, but ${info.args} descriptions attached.")
          val isSpecialFunc = predefFuncRegistry.specialFuncs.get(f.name).isDefined
          Try{assert(predefFuncRegistry.funcs.get(f.name).isDefined || isSpecialFunc)}
              .fold(t => throw new RuntimeException(s"Cannot resolve func $f using predefFuncRegistry.funcs(${f.name})", t), _ => ())
          val args = info.args.map { a =>
            s"""val ${a.name}Arg: ArgInfo = func.argInfo("${a.name}")"""
          }
          s"""
            |  object ${opName}Info extends InfoObject {
            |    private val func = predefinedOps.${ if (isSpecialFunc) "specialFuncs" else "funcs" }("${f.name}")
            |    ${args.rep(sep = "\n    ")}
            |    val argInfos: Seq[ArgInfo] = Seq(${info.args.rep(a => s"${a.name}Arg")})
            |  }
           """.stripMargin
      }
      res
    }
    val infoText = infoStrings.rep(sep = "")
    println(infoText)
    println(s"Total infos: ${infoStrings.length}")
  }

}
