package sigmastate

import org.ergoplatform.ErgoBox.RegisterId
import scalan.reflection.CommonReflection.registerClassEntry
import scalan.reflection.{SRConstructor, SRMethod}
import sigmastate.SAvlTree.KeyValueColl
import sigmastate.SCollection.{SIntArray, SByteArray, SBooleanArray}
import sigmastate.Values._
import sigmastate.basics.VerifierMessage.Challenge
import sigmastate.crypto.GF2_192_Poly
import sigmastate.interpreter.ErgoTreeEvaluator
import sigmastate.lang.Terms._
import sigmastate.lang.{CheckingSigmaBuilder, Terms}
import sigmastate.serialization.OpCodes.OpCode
import sigmastate.utxo._
import special.collection.Coll
import special.sigma.{SigmaDslBuilder, AvlTree}

object InterpreterReflection {
//  registerClassEntry(classOf[sigmastate.AND],
//    constructors = Array(
//      new SRConstructor[Any](Array(classOf[Value[_]])) {
//        override def newInstance(args: AnyRef*): Any =
//          new sigmastate.AND(args(0).asInstanceOf[Value[SBooleanArray]])
//      }
//    )
//  )
//
//  registerClassEntry(classOf[ArithOp[_]],
//    constructors = Array(
//      new SRConstructor[Any](Array(classOf[Value[_]], classOf[Value[_]], classOf[Byte])) {
//        override def newInstance(args: AnyRef*): Any =
//          new ArithOp(args(0).asInstanceOf[Value[SType]], args(1).asInstanceOf[Value[SType]], args(2).asInstanceOf[OpCode])
//      }
//    )
//  )
//
//  registerClassEntry(classOf[AtLeast],
//    constructors = Array(
//      new SRConstructor[Any](Array(classOf[Value[_]], classOf[Value[_]])) {
//        override def newInstance(args: AnyRef*): Any =
//          new AtLeast(args(0).asInstanceOf[Value[SInt.type]], args(1).asInstanceOf[Value[SCollection[SSigmaProp.type]]])
//      }
//    )
//  )
//
//  registerClassEntry(classOf[BinAnd],
//    constructors = Array(
//      new SRConstructor[Any](Array(classOf[Value[_]], classOf[Value[_]])) {
//        override def newInstance(args: AnyRef*): Any =
//          new BinAnd(args(0).asInstanceOf[BoolValue], args(1).asInstanceOf[BoolValue])
//      }
//    )
//  )
//
//  registerClassEntry(classOf[BinOr],
//    constructors = Array(
//      new SRConstructor[Any](Array(classOf[Value[_]], classOf[Value[_]])) {
//        override def newInstance(args: AnyRef*): Any =
//          new BinOr(args(0).asInstanceOf[BoolValue], args(1).asInstanceOf[BoolValue])
//      }
//    )
//  )
//
//  registerClassEntry(classOf[BinXor],
//    constructors = Array(
//      new SRConstructor[Any](Array(classOf[Value[_]], classOf[Value[_]])) {
//        override def newInstance(args: AnyRef*): Any =
//          new BinXor(args(0).asInstanceOf[BoolValue], args(1).asInstanceOf[BoolValue])
//      }
//    )
//  )
//
//  registerClassEntry(classOf[BoolToSigmaProp],
//    constructors = Array(
//      new SRConstructor[Any](Array(classOf[Value[_]])) {
//        override def newInstance(args: AnyRef*): Any =
//          new BoolToSigmaProp(args(0).asInstanceOf[BoolValue])
//      }
//    )
//  )
//
//  registerClassEntry(classOf[ByteArrayToBigInt],
//    constructors = Array(
//      new SRConstructor[Any](Array(classOf[Value[_]])) {
//        override def newInstance(args: AnyRef*): Any =
//          new ByteArrayToBigInt(args(0).asInstanceOf[Value[SByteArray]])
//      }
//    )
//  )
//
//  registerClassEntry(classOf[CAndUncheckedNode],
//    constructors = Array(
//      new SRConstructor[Any](Array(classOf[Array[Byte]], classOf[Seq[_]])) {
//        override def newInstance(args: AnyRef*): Any =
//          CAndUncheckedNode(args(0).asInstanceOf[Challenge], args(1).asInstanceOf[Seq[UncheckedSigmaTree]])
//      }
//    )
//  )
//
//  registerClassEntry(classOf[CAndUnproven],
//    constructors = Array(
//      new SRConstructor[Any](Array(classOf[CAND], classOf[scala.Option[_]], classOf[Boolean], classOf[Seq[_]], classOf[NodePosition])) {
//        override def newInstance(args: AnyRef*): Any =
//          CAndUnproven(args(0).asInstanceOf[CAND], args(1).asInstanceOf[scala.Option[Challenge]], args(2).asInstanceOf[Boolean], args(3).asInstanceOf[Seq[ProofTree]], args(4).asInstanceOf[NodePosition])
//      }
//    )
//  )
//
//  registerClassEntry(classOf[COrUncheckedNode],
//    constructors = Array(
//      new SRConstructor[Any](Array(classOf[Array[Byte]], classOf[Seq[_]])) {
//        override def newInstance(args: AnyRef*): Any =
//          COrUncheckedNode(args(0).asInstanceOf[Challenge], args(1).asInstanceOf[Seq[UncheckedSigmaTree]])
//      }
//    )
//  )
//
//  registerClassEntry(classOf[COrUnproven],
//    constructors = Array(
//      new SRConstructor[Any](Array(classOf[COR], classOf[scala.Option[_]], classOf[Boolean], classOf[Seq[_]], classOf[NodePosition])) {
//        override def newInstance(args: AnyRef*): Any =
//          COrUnproven(args(0).asInstanceOf[COR], args(1).asInstanceOf[scala.Option[Challenge]], args(2).asInstanceOf[Boolean], args(3).asInstanceOf[Seq[ProofTree]], args(4).asInstanceOf[NodePosition])
//      }
//    )
//  )
//
//  registerClassEntry(classOf[CThresholdUncheckedNode],
//    constructors = Array(
//      new SRConstructor[Any](Array(classOf[Array[Byte]], classOf[Seq[_]], classOf[java.lang.Integer], classOf[scala.Option[_]])) {
//        override def newInstance(args: AnyRef*): Any =
//          CThresholdUncheckedNode(args(0).asInstanceOf[Challenge], args(1).asInstanceOf[Seq[UncheckedSigmaTree]], args(2).asInstanceOf[java.lang.Integer], args(3).asInstanceOf[scala.Option[GF2_192_Poly]])
//      }
//    )
//  )
//
//  registerClassEntry(classOf[CThresholdUnproven],
//    constructors = Array(
//      new SRConstructor[Any](Array(classOf[CTHRESHOLD], classOf[scala.Option[_]], classOf[Boolean], classOf[java.lang.Integer], classOf[Seq[_]], classOf[scala.Option[_]], classOf[NodePosition])) {
//        override def newInstance(args: AnyRef*): Any =
//          CThresholdUnproven(args(0).asInstanceOf[CTHRESHOLD], args(1).asInstanceOf[scala.Option[Challenge]], args(2).asInstanceOf[Boolean], args(3).asInstanceOf[java.lang.Integer], args(4).asInstanceOf[Seq[ProofTree]], args(5).asInstanceOf[scala.Option[GF2_192_Poly]], args(6).asInstanceOf[NodePosition])
//      }
//    )
//  )
//
//  registerClassEntry(classOf[CalcBlake2b256],
//    constructors = Array(
//      new SRConstructor[Any](Array(classOf[Value[_]])) {
//        override def newInstance(args: AnyRef*): Any =
//          new CalcBlake2b256(args(0).asInstanceOf[Value[SByteArray]])
//      }
//    )
//  )
//
//  registerClassEntry(classOf[CalcSha256],
//    constructors = Array(
//      new SRConstructor[Any](Array(classOf[Value[_]])) {
//        override def newInstance(args: AnyRef*): Any =
//          new CalcSha256(args(0).asInstanceOf[Value[SByteArray]])
//      }
//    )
//  )
//
//  registerClassEntry(classOf[CreateProveDHTuple],
//    constructors = Array(
//      new SRConstructor[Any](Array(classOf[Value[_]], classOf[Value[_]], classOf[Value[_]], classOf[Value[_]])) {
//        override def newInstance(args: AnyRef*): Any =
//          new CreateProveDHTuple(args(0).asInstanceOf[GroupElementValue], args(1).asInstanceOf[GroupElementValue], args(2).asInstanceOf[GroupElementValue], args(3).asInstanceOf[GroupElementValue])
//      }
//    )
//  )
//
//  registerClassEntry(classOf[Downcast[_, _]],
//    constructors = Array(
//      new SRConstructor[Any](Array(classOf[Value[_]], classOf[SNumericType])) {
//        override def newInstance(args: AnyRef*): Any =
//          new Downcast(args(0).asInstanceOf[Value[SNumericType]], args(1).asInstanceOf[SNumericType])
//      }
//    )
//  )
//
//  registerClassEntry(classOf[EQ[_]],
//    constructors = Array(
//      new SRConstructor[Any](Array(classOf[Value[_]], classOf[Value[_]])) {
//        override def newInstance(args: AnyRef*): Any =
//          new EQ(args(0).asInstanceOf[Value[SType]], args(1).asInstanceOf[Value[SType]])
//      }
//    )
//  )
//
//  registerClassEntry(classOf[Exponentiate],
//    constructors = Array(
//      new SRConstructor[Any](Array(classOf[Value[_]], classOf[Value[_]])) {
//        override def newInstance(args: AnyRef*): Any =
//          new Exponentiate(args(0).asInstanceOf[GroupElementValue], args(1).asInstanceOf[BigIntValue])
//      }
//    )
//  )
//
//  registerClassEntry(classOf[GE[_]],
//    constructors = Array(
//      new SRConstructor[Any](Array(classOf[Value[_]], classOf[Value[_]])) {
//        override def newInstance(args: AnyRef*): Any =
//          new GE(args(0).asInstanceOf[Value[SType]], args(1).asInstanceOf[Value[SType]])
//      }
//    )
//  )
//
//  registerClassEntry(classOf[GT[_]],
//    constructors = Array(
//      new SRConstructor[Any](Array(classOf[Value[_]], classOf[Value[_]])) {
//        override def newInstance(args: AnyRef*): Any =
//          new GT(args(0).asInstanceOf[Value[SType]], args(1).asInstanceOf[Value[SType]])
//      }
//    )
//  )
//
//  registerClassEntry(classOf[If[_]],
//    constructors = Array(
//      new SRConstructor[Any](Array(classOf[Value[_]], classOf[Value[_]], classOf[Value[_]])) {
//        override def newInstance(args: AnyRef*): Any =
//          new If(args(0).asInstanceOf[BoolValue], args(1).asInstanceOf[Value[SType]], args(2).asInstanceOf[Value[SType]])
//      }
//    )
//  )
//
//  registerClassEntry(classOf[LE[_]],
//    constructors = Array(
//      new SRConstructor[Any](Array(classOf[Value[_]], classOf[Value[_]])) {
//        override def newInstance(args: AnyRef*): Any =
//          new LE(args(0).asInstanceOf[Value[SType]], args(1).asInstanceOf[Value[SType]])
//      }
//    )
//  )
//
//  registerClassEntry(classOf[LT[_]],
//    constructors = Array(
//      new SRConstructor[Any](Array(classOf[Value[_]], classOf[Value[_]])) {
//        override def newInstance(args: AnyRef*): Any =
//          new LT(args(0).asInstanceOf[Value[SType]], args(1).asInstanceOf[Value[SType]])
//      }
//    )
//  )
//
//  registerClassEntry(classOf[LogicalNot],
//    constructors = Array(
//      new SRConstructor[Any](Array(classOf[Value[_]])) {
//        override def newInstance(args: AnyRef*): Any =
//          new LogicalNot(args(0).asInstanceOf[BoolValue])
//      }
//    )
//  )
//
//  registerClassEntry(classOf[MultiplyGroup],
//    constructors = Array(
//      new SRConstructor[Any](Array(classOf[Value[_]], classOf[Value[_]])) {
//        override def newInstance(args: AnyRef*): Any =
//          new MultiplyGroup(args(0).asInstanceOf[GroupElementValue], args(1).asInstanceOf[GroupElementValue])
//      }
//    )
//  )
//
//  registerClassEntry(classOf[NEQ[_]],
//    constructors = Array(
//      new SRConstructor[Any](Array(classOf[Value[_]], classOf[Value[_]])) {
//        override def newInstance(args: AnyRef*): Any =
//          new NEQ(args(0).asInstanceOf[Value[SType]], args(1).asInstanceOf[Value[SType]])
//      }
//    )
//  )
//
//  registerClassEntry(classOf[Negation[_]],
//    constructors = Array(
//      new SRConstructor[Any](Array(classOf[Value[_]])) {
//        override def newInstance(args: AnyRef*): Any =
//          new Negation(args(0).asInstanceOf[Value[SType]])
//      }
//    )
//  )
//
//  registerClassEntry(classOf[OR],
//    constructors = Array(
//      new SRConstructor[Any](Array(classOf[Value[_]])) {
//        override def newInstance(args: AnyRef*): Any =
//          new OR(args(0).asInstanceOf[Value[SBooleanArray]])
//      }
//    )
//  )
//
//  { val clazz = SAvlTree.getClass
//    registerClassEntry(clazz,
//      methods = Map(
//      {
//        val paramTypes: Seq[Class[_]] = Array(classOf[MethodCall], classOf[AvlTree], classOf[Coll[_]], classOf[Coll[_]], classOf[ErgoTreeEvaluator])
//        ("update_eval", paramTypes) ->
//            new SRMethod(clazz, "update_eval", paramTypes) {
//              override def invoke(obj: Any, args: AnyRef*): AnyRef = {
//                SAvlTree.update_eval(
//                  args(0).asInstanceOf[Terms.MethodCall],
//                  args(1).asInstanceOf[AvlTree],
//                  args(2).asInstanceOf[KeyValueColl],
//                  args(3).asInstanceOf[Coll[Byte]])(args(4).asInstanceOf[ErgoTreeEvaluator])
//              }
//            }
//      },
//      {
//        val paramTypes: Seq[Class[_]] = Array(classOf[MethodCall], classOf[AvlTree], classOf[Coll[_]], classOf[Coll[_]], classOf[ErgoTreeEvaluator])
//        ("get_eval", paramTypes) ->
//            new SRMethod(clazz, "get_eval", paramTypes) {
//              override def invoke(obj: Any, args: AnyRef*): AnyRef = {
//                SAvlTree.get_eval(args(0).asInstanceOf[MethodCall], args(1).asInstanceOf[AvlTree], args(2).asInstanceOf[Coll[Byte]], args(3).asInstanceOf[Coll[Byte]])(args(4).asInstanceOf[ErgoTreeEvaluator])
//              }
//            }
//      },
//      {
//        val paramTypes: Seq[Class[_]] = Array(classOf[MethodCall], classOf[AvlTree], classOf[Coll[_]], classOf[Coll[_]], classOf[ErgoTreeEvaluator])
//        ("insert_eval", paramTypes) ->
//            new SRMethod(clazz, "insert_eval", paramTypes) {
//              override def invoke(obj: Any, args: AnyRef*): AnyRef = {
//                SAvlTree.insert_eval(args(0).asInstanceOf[MethodCall],
//                  args(1).asInstanceOf[AvlTree],
//                  args(2).asInstanceOf[KeyValueColl],
//                  args(3).asInstanceOf[Coll[Byte]])(args(4).asInstanceOf[ErgoTreeEvaluator])
//              }
//            }
//      },
//      {
//        val paramTypes: Seq[Class[_]] = Array(classOf[MethodCall], classOf[AvlTree], classOf[Coll[_]], classOf[Coll[_]], classOf[ErgoTreeEvaluator])
//        ("getMany_eval", paramTypes) ->
//            new SRMethod(clazz, "getMany_eval", paramTypes) {
//              override def invoke(obj: Any, args: AnyRef*): AnyRef = {
//                SAvlTree.getMany_eval(args(0).asInstanceOf[MethodCall],
//                  args(1).asInstanceOf[AvlTree],
//                  args(2).asInstanceOf[Coll[Coll[Byte]]],
//                  args(3).asInstanceOf[Coll[Byte]])(args(4).asInstanceOf[ErgoTreeEvaluator])
//              }
//            }
//      },
//      {
//        val paramTypes: Seq[Class[_]] = Array(classOf[MethodCall], classOf[AvlTree], classOf[Coll[_]], classOf[Coll[_]], classOf[ErgoTreeEvaluator])
//        ("remove_eval", paramTypes) ->
//            new SRMethod(clazz, "remove_eval", paramTypes) {
//              override def invoke(obj: Any, args: AnyRef*): AnyRef = {
//                SAvlTree.remove_eval(args(0).asInstanceOf[MethodCall],
//                  args(1).asInstanceOf[AvlTree],
//                  args(2).asInstanceOf[Coll[Coll[Byte]]],
//                  args(3).asInstanceOf[Coll[Byte]])(args(4).asInstanceOf[ErgoTreeEvaluator])
//              }
//            }
//      },
//      {
//        val paramTypes: Seq[Class[_]] = Array(classOf[MethodCall], classOf[AvlTree], classOf[Coll[_]], classOf[Coll[_]], classOf[ErgoTreeEvaluator])
//        ("contains_eval", paramTypes) ->
//            new SRMethod(clazz, "contains_eval", paramTypes) {
//              override def invoke(obj: Any, args: AnyRef*): AnyRef = {
//                SAvlTree.contains_eval(args(0).asInstanceOf[MethodCall],
//                  args(1).asInstanceOf[AvlTree],
//                  args(2).asInstanceOf[Coll[Byte]],
//                  args(3).asInstanceOf[Coll[Byte]])(args(4).asInstanceOf[ErgoTreeEvaluator]).asInstanceOf[AnyRef]
//              }
//            }
//      }
//      )
//    )
//  }
//
//  { val clazz = SCollection.getClass
//    registerClassEntry(clazz,
//      methods = Map(
//      {
//        val paramTypes: Seq[Class[_]] = Array(classOf[MethodCall], classOf[Coll[_]], classOf[Function1[_, _]], classOf[ErgoTreeEvaluator])
//        ("map_eval", paramTypes) ->
//            new SRMethod(clazz, "map_eval", paramTypes) {
//              override def invoke(obj: Any, args: AnyRef*): AnyRef = {
//                SCollection.map_eval(args(0).asInstanceOf[MethodCall],
//                  args(1).asInstanceOf[Coll[Any]],
//                  args(2).asInstanceOf[Any => Any])(args(3).asInstanceOf[ErgoTreeEvaluator])
//              }
//            }
//      },
//      {
//        val paramTypes: Seq[Class[_]] = Array(classOf[MethodCall], classOf[Coll[_]], classOf[Function1[_, _]], classOf[ErgoTreeEvaluator])
//        ("flatMap_eval", paramTypes) ->
//            new SRMethod(clazz, "flatMap_eval", paramTypes) {
//              override def invoke(obj: Any, args: AnyRef*): AnyRef = {
//                SCollection.flatMap_eval(args(0).asInstanceOf[MethodCall],
//                  args(1).asInstanceOf[Coll[Any]],
//                  args(2).asInstanceOf[Any => Coll[Any]])(args(3).asInstanceOf[ErgoTreeEvaluator])
//              }
//            }
//      },
//      {
//        val paramTypes: Seq[Class[_]] = Array(classOf[MethodCall], classOf[Coll[_]], classOf[Coll[_]], classOf[Coll[_]], classOf[ErgoTreeEvaluator])
//        ("updateMany_eval", paramTypes) ->
//            new SRMethod(clazz, "updateMany_eval", paramTypes) {
//              override def invoke(obj: Any, args: AnyRef*): AnyRef = {
//                SCollection.updateMany_eval(args(0).asInstanceOf[MethodCall],
//                  args(1).asInstanceOf[Coll[Any]],
//                  args(2).asInstanceOf[Coll[Int]],
//                  args(3).asInstanceOf[Coll[Any]])(args(4).asInstanceOf[ErgoTreeEvaluator])
//              }
//            }
//      },
//      {
//        val paramTypes: Seq[Class[_]] = Array(classOf[MethodCall], classOf[Coll[_]], classOf[java.lang.Object], classOf[Int], classOf[ErgoTreeEvaluator])
//        ("indexOf_eval", paramTypes) ->
//            new SRMethod(clazz, "indexOf_eval", paramTypes) {
//              override def invoke(obj: Any, args: AnyRef*): AnyRef = {
//                SCollection.indexOf_eval(args(0).asInstanceOf[MethodCall],
//                  args(1).asInstanceOf[Coll[Any]], args(2), args(3).asInstanceOf[Int]
//                )(args(4).asInstanceOf[ErgoTreeEvaluator]).asInstanceOf[AnyRef]
//              }
//            }
//      },
//      {
//        val paramTypes: Seq[Class[_]] = Array(classOf[MethodCall], classOf[Coll[_]], classOf[ErgoTreeEvaluator])
//        ("indices_eval", paramTypes) ->
//            new SRMethod(clazz, "indices_eval", paramTypes) {
//              override def invoke(obj: Any, args: AnyRef*): AnyRef = {
//                SCollection.indices_eval(args(0).asInstanceOf[MethodCall],
//                  args(1).asInstanceOf[Coll[Any]])(args(2).asInstanceOf[ErgoTreeEvaluator])
//              }
//            }
//      },
//      {
//        val paramTypes: Seq[Class[_]] = Array(classOf[MethodCall], classOf[Coll[_]], classOf[Int], classOf[java.lang.Object], classOf[ErgoTreeEvaluator])
//        ("updated_eval", paramTypes) ->
//            new SRMethod(clazz, "updated_eval", paramTypes) {
//              override def invoke(obj: Any, args: AnyRef*): AnyRef = {
//                SCollection.updated_eval(args(0).asInstanceOf[MethodCall],
//                  args(1).asInstanceOf[Coll[Any]],
//                  args(2).asInstanceOf[Int],
//                  args(3))(args(4).asInstanceOf[ErgoTreeEvaluator])
//              }
//            }
//      },
//      {
//        val paramTypes: Seq[Class[_]] = Array(classOf[MethodCall], classOf[Coll[_]], classOf[Int], classOf[Coll[_]], classOf[Int], classOf[ErgoTreeEvaluator])
//        ("patch_eval", paramTypes) ->
//            new SRMethod(clazz, "patch_eval", paramTypes) {
//              override def invoke(obj: Any, args: AnyRef*): AnyRef = {
//                SCollection.patch_eval(args(0).asInstanceOf[MethodCall],
//                  args(1).asInstanceOf[Coll[Any]],
//                  args(2).asInstanceOf[Int],
//                  args(3).asInstanceOf[Coll[Any]],
//                  args(4).asInstanceOf[Int])(args(5).asInstanceOf[ErgoTreeEvaluator])
//              }
//            }
//      },
//      {
//        val paramTypes: Seq[Class[_]] = Array(classOf[MethodCall], classOf[Coll[_]], classOf[Coll[_]], classOf[ErgoTreeEvaluator])
//        ("zip_eval", paramTypes) ->
//            new SRMethod(clazz, "zip_eval", paramTypes) {
//              override def invoke(obj: Any, args: AnyRef*): AnyRef = {
//                SCollection.zip_eval(args(0).asInstanceOf[MethodCall],
//                  args(1).asInstanceOf[Coll[Any]],
//                  args(2).asInstanceOf[Coll[Any]])(args(3).asInstanceOf[ErgoTreeEvaluator])
//              }
//            }
//      },
//      {
//        val paramTypes: Seq[Class[_]] = Array(classOf[MethodCall], classOf[Coll[_]], classOf[Int], classOf[java.lang.Object], classOf[ErgoTreeEvaluator])
//        ("getOrElse_eval", paramTypes) ->
//            new SRMethod(clazz, "getOrElse_eval", paramTypes) {
//              override def invoke(obj: Any, args: AnyRef*): AnyRef = {
//                SCollection.getOrElse_eval(args(0).asInstanceOf[MethodCall],
//                  args(1).asInstanceOf[Coll[Any]],
//                  args(2).asInstanceOf[Int], args(3)
//                )(args(4).asInstanceOf[ErgoTreeEvaluator]).asInstanceOf[AnyRef]
//              }
//            }
//      }
//      )
//    )
//  }
//
//  registerClassEntry(classOf[sigmastate.SCollectionType[_]],
//    constructors = Array(
//      new SRConstructor[Any](Array(classOf[SType])) {
//        override def newInstance(args: AnyRef*): Any =
//          new sigmastate.SCollectionType(args(0).asInstanceOf[SType])
//      }
//    )
//  )
//
//  { val clazz = SGlobal.getClass
//    registerClassEntry(clazz,
//      methods = Map(
//      {
//        val paramTypes: Seq[Class[_]] = Array(classOf[MethodCall], classOf[SigmaDslBuilder], classOf[Coll[_]], classOf[Coll[_]], classOf[ErgoTreeEvaluator])
//        ("xor_eval", paramTypes) ->
//            new SRMethod(clazz, "xor_eval", paramTypes) {
//              override def invoke(obj: Any, args: AnyRef*): AnyRef = {
//                SGlobal.xor_eval(args(0).asInstanceOf[MethodCall],
//                  args(1).asInstanceOf[SigmaDslBuilder],
//                  args(2).asInstanceOf[Coll[Byte]],
//                  args(3).asInstanceOf[Coll[Byte]])(args(4).asInstanceOf[ErgoTreeEvaluator])
//              }
//            }
//      }
//      )
//    )
//  }
//
//  registerClassEntry(classOf[SOption[_]],
//    constructors = Array(
//      new SRConstructor[Any](Array(classOf[SType])) {
//        override def newInstance(args: AnyRef*): Any =
//          new sigmastate.SOption(args(0).asInstanceOf[SType])
//      }
//    )
//  )
//
//  registerClassEntry(classOf[sigmastate.STuple],
//    constructors = Array(
//      new SRConstructor[Any](Array(classOf[IndexedSeq[_]])) {
//        override def newInstance(args: AnyRef*): Any =
//          new sigmastate.STuple(args(0).asInstanceOf[IndexedSeq[SType]])
//      }
//    )
//  )
//
//  registerClassEntry(classOf[sigmastate.SigmaAnd],
//    constructors = Array(
//      new SRConstructor[Any](Array(classOf[Seq[_]])) {
//        override def newInstance(args: AnyRef*): Any =
//          new sigmastate.SigmaAnd(args(0).asInstanceOf[Seq[SigmaPropValue]])
//      }
//    )
//  )
//
//  registerClassEntry(classOf[sigmastate.SigmaOr],
//    constructors = Array(
//      new SRConstructor[Any](Array(classOf[Seq[_]])) {
//        override def newInstance(args: AnyRef*): Any =
//          new sigmastate.SigmaOr(args(0).asInstanceOf[Seq[SigmaPropValue]])
//      }
//    )
//  )
//
//  registerClassEntry(classOf[SubstConstants[_]],
//    constructors = Array(
//      new SRConstructor[Any](Array(classOf[Value[_]], classOf[Value[_]], classOf[Value[_]])) {
//        override def newInstance(args: AnyRef*): AnyRef =
//          new SubstConstants[SAny.type](args(0).asInstanceOf[Value[SByteArray]],
//            args(1).asInstanceOf[Value[SIntArray]],
//            args(2).asInstanceOf[Value[SCollection[SAny.type]]])
//      }
//    )
//  )
//
//  registerClassEntry(classOf[sigmastate.Upcast[_, _]],
//    constructors = Array(
//      new SRConstructor[Any](Array(classOf[Value[_]], classOf[SNumericType])) {
//        override def newInstance(args: AnyRef*): Any =
//          new sigmastate.Upcast(args(0).asInstanceOf[Value[SNumericType]], args(1).asInstanceOf[SNumericType])
//      }
//    )
//  )
//
//  registerClassEntry(classOf[BlockValue],
//    constructors = Array(
//      new SRConstructor[Any](Array(classOf[IndexedSeq[_]], classOf[Value[_]])) {
//        override def newInstance(args: AnyRef*): Any =
//          new BlockValue(args(0).asInstanceOf[IndexedSeq[BlockItem]], args(1).asInstanceOf[SValue])
//      }
//    )
//  )
//
//  registerClassEntry(classOf[ConcreteCollection[_]],
//    constructors = Array(
//      new SRConstructor[Any](Array(classOf[Seq[_]], classOf[SType])) {
//        override def newInstance(args: AnyRef*): Any =
//          new ConcreteCollection(args(0).asInstanceOf[Seq[SValue]], args(1).asInstanceOf[SType])
//      }
//    )
//  )
//
//  registerClassEntry(classOf[FuncValue],
//    constructors = Array(
//      new SRConstructor[Any](Array(classOf[IndexedSeq[_]], classOf[Value[_]])) {
//        override def newInstance(args: AnyRef*): Any =
//          new FuncValue(args(0).asInstanceOf[IndexedSeq[(Int, SType)]], args(1).asInstanceOf[SValue])
//      }
//    )
//  )
//
//  registerClassEntry(classOf[Tuple],
//    constructors = Array(
//      new SRConstructor[Any](Array(classOf[IndexedSeq[_]])) {
//        override def newInstance(args: AnyRef*): Any =
//          new Tuple(args(0).asInstanceOf[IndexedSeq[SValue]])
//      }
//    )
//  )
//
//  registerClassEntry(classOf[ValDef],
//    constructors = Array(
//      new SRConstructor[Any](Array(classOf[Int], classOf[Seq[_]], classOf[Value[_]])) {
//        override def newInstance(args: AnyRef*): Any =
//          new ValDef(args(0).asInstanceOf[Int],
//            args(1).asInstanceOf[Seq[STypeVar]],
//            args(2).asInstanceOf[SValue])
//      }
//    )
//  )
//
//  registerClassEntry(classOf[Apply],
//    constructors = Array(
//      new SRConstructor[Any](Array(classOf[Value[_]], classOf[IndexedSeq[_]])) {
//        override def newInstance(args: AnyRef*): Any =
//          new Apply(args(0).asInstanceOf[SValue], args(1).asInstanceOf[IndexedSeq[SValue]])
//      }
//    )
//  )
//
//  registerClassEntry(classOf[ApplyTypes],
//    constructors = Array(
//      new SRConstructor[Any](Array(classOf[Value[_]], classOf[Seq[_]])) {
//        override def newInstance(args: AnyRef*): Any =
//          new ApplyTypes(args(0).asInstanceOf[SValue], args(1).asInstanceOf[Seq[SType]])
//      }
//    )
//  )
//
//  registerClassEntry(classOf[Block],
//    constructors = Array(
//      new SRConstructor[Any](Array(classOf[Seq[_]], classOf[Value[_]])) {
//        override def newInstance(args: AnyRef*): Any =
//          new Block(args(0).asInstanceOf[Seq[Val]], args(1).asInstanceOf[SValue])
//      }
//    )
//  )
//
//  registerClassEntry(classOf[Lambda],
//    constructors = Array(
//      new SRConstructor[Any](Array(classOf[Seq[_]], classOf[IndexedSeq[_]], classOf[SType], classOf[scala.Option[_]])) {
//        override def newInstance(args: AnyRef*): Any =
//          new Lambda(args(0).asInstanceOf[Seq[STypeParam]],
//            args(1).asInstanceOf[IndexedSeq[(String, SType)]],
//            args(2).asInstanceOf[SType], args(3).asInstanceOf[scala.Option[SValue]])
//      }
//    )
//  )
//
//  registerClassEntry(classOf[MethodCall],
//    constructors = Array(
//      new SRConstructor[Any](Array(classOf[Value[_]], classOf[SMethod], classOf[IndexedSeq[_]], classOf[scala.collection.immutable.Map[_, _]])) {
//        override def newInstance(args: AnyRef*): Any =
//          new MethodCall(args(0).asInstanceOf[SValue],
//            args(1).asInstanceOf[SMethod], args(2).asInstanceOf[IndexedSeq[SValue]],
//            args(3).asInstanceOf[scala.collection.immutable.Map[STypeVar, SType]])
//      }
//    )
//  )
//
//  registerClassEntry(classOf[MethodCallLike],
//    constructors = Array(
//      new SRConstructor[Any](Array(classOf[Value[_]], classOf[String], classOf[IndexedSeq[_]], classOf[SType])) {
//        override def newInstance(args: AnyRef*): Any =
//          new MethodCallLike(args(0).asInstanceOf[SValue],
//            args(1).asInstanceOf[String],
//            args(2).asInstanceOf[IndexedSeq[SValue]],
//            args(3).asInstanceOf[SType])
//      }
//    )
//  )
//
//  registerClassEntry(classOf[Select],
//    constructors = Array(
//      new SRConstructor[Any](Array(classOf[Value[_]], classOf[String], classOf[scala.Option[_]])) {
//        override def newInstance(args: AnyRef*): Any =
//          new Select(args(0).asInstanceOf[SValue],
//            args(1).asInstanceOf[String],
//            args(2).asInstanceOf[scala.Option[SType]])
//      }
//    )
//  )
//
//  registerClassEntry(classOf[ValNode],
//    constructors = Array(
//      new SRConstructor[Any](Array(classOf[String], classOf[SType], classOf[Value[_]])) {
//        override def newInstance(args: AnyRef*): Any =
//          new ValNode(args(0).asInstanceOf[String], args(1).asInstanceOf[SType], args(2).asInstanceOf[SValue])
//      }
//    )
//  )
//
//  registerClassEntry(classOf[Append[_]],
//    constructors = Array(
//      new SRConstructor[Any](Array(classOf[Value[_]], classOf[Value[_]])) {
//        override def newInstance(args: AnyRef*): Any =
//          new Append(args(0).asInstanceOf[CollectionValue[SType]], args(1).asInstanceOf[CollectionValue[SType]])
//      }
//    )
//  )
//
//  registerClassEntry(classOf[ByIndex[_]],
//    constructors = Array(
//      new SRConstructor[Any](Array(classOf[Value[_]], classOf[Value[_]], classOf[scala.Option[_]])) {
//        override def newInstance(args: AnyRef*): Any =
//          new ByIndex[SAny.type](args(0).asInstanceOf[CollectionValue[SAny.type]],
//            args(1).asInstanceOf[IntValue],
//            args(2).asInstanceOf[scala.Option[Value[SAny.type]]])
//      }
//    )
//  )
//
//  registerClassEntry(classOf[Exists[_]],
//    constructors = Array(
//      new SRConstructor[Any](Array(classOf[Value[_]], classOf[Value[_]])) {
//        override def newInstance(args: AnyRef*): Any =
//          new Exists(args(0).asInstanceOf[CollectionValue[SAny.type]], args(1).asInstanceOf[Value[SFunc]])
//      }
//    )
//  )
//
//  registerClassEntry(classOf[ExtractAmount],
//    constructors = Array(
//      new SRConstructor[Any](Array(classOf[Value[_]])) {
//        override def newInstance(args: AnyRef*): Any =
//          new ExtractAmount(args(0).asInstanceOf[BoxValue])
//      }
//    )
//  )
//
//  registerClassEntry(classOf[ExtractBytesWithNoRef],
//    constructors = Array(
//      new SRConstructor[Any](Array(classOf[Value[_]])) {
//        override def newInstance(args: AnyRef*): Any =
//          new ExtractBytesWithNoRef(args(0).asInstanceOf[BoxValue])
//      }
//    )
//  )
//
//  registerClassEntry(classOf[ExtractCreationInfo],
//    constructors = Array(
//      new SRConstructor[Any](Array(classOf[Value[_]])) {
//        override def newInstance(args: AnyRef*): Any =
//          new ExtractCreationInfo(args(0).asInstanceOf[BoxValue])
//      }
//    )
//  )
//
//  registerClassEntry(classOf[ExtractId],
//    constructors = Array(
//      new SRConstructor[Any](Array(classOf[Value[_]])) {
//        override def newInstance(args: AnyRef*): Any =
//          new ExtractId(args(0).asInstanceOf[BoxValue])
//      }
//    )
//  )
//
//  registerClassEntry(classOf[ExtractRegisterAs[_]],
//    constructors = Array(
//      new SRConstructor[Any](Array(classOf[Value[_]], classOf[RegisterId], classOf[SOption[_]])) {
//        override def newInstance(args: AnyRef*): Any =
//          new ExtractRegisterAs(args(0).asInstanceOf[BoxValue],
//            args(1).asInstanceOf[RegisterId],
//            args(2).asInstanceOf[SOption[SAny.type]])
//      }
//    )
//  )
//
//  registerClassEntry(classOf[ExtractScriptBytes],
//    constructors = Array(
//      new SRConstructor[Any](Array(classOf[Value[_]])) {
//        override def newInstance(args: AnyRef*): Any =
//          new ExtractScriptBytes(args(0).asInstanceOf[BoxValue])
//      }
//    )
//  )
//
//  registerClassEntry(classOf[Filter[_]],
//    constructors = Array(
//      new SRConstructor[Any](Array(classOf[Value[_]], classOf[Value[_]])) {
//        override def newInstance(args: AnyRef*): Any =
//          new Filter(args(0).asInstanceOf[CollectionValue[SAny.type]], args(1).asInstanceOf[Value[SFunc]])
//      }
//    )
//  )
//
//  registerClassEntry(classOf[Fold[_, _]],
//    constructors = Array(
//      new SRConstructor[Any](Array(classOf[Value[_]], classOf[Value[_]], classOf[Value[_]])) {
//        override def newInstance(args: AnyRef*): Any =
//          new Fold(args(0).asInstanceOf[CollectionValue[SAny.type]],
//            args(1).asInstanceOf[SValue], args(2).asInstanceOf[Value[SFunc]])
//      }
//    )
//  )
//
//  registerClassEntry(classOf[ForAll[_]],
//    constructors = Array(
//      new SRConstructor[Any](Array(classOf[Value[_]], classOf[Value[_]])) {
//        override def newInstance(args: AnyRef*): Any =
//          new ForAll(args(0).asInstanceOf[CollectionValue[SAny.type]],
//            args(1).asInstanceOf[Value[SFunc]])
//      }
//    )
//  )
//
//  registerClassEntry(classOf[MapCollection[_, _]],
//    constructors = Array(
//      new SRConstructor[Any](Array(classOf[Value[_]], classOf[Value[_]])) {
//        override def newInstance(args: AnyRef*): Any =
//          new MapCollection(args(0).asInstanceOf[CollectionValue[SAny.type]],
//            args(1).asInstanceOf[Value[SFunc]])
//      }
//    )
//  )
//
//  registerClassEntry(classOf[OptionGet[_]],
//    constructors = Array(
//      new SRConstructor[Any](Array(classOf[Value[_]])) {
//        override def newInstance(args: AnyRef*): Any =
//          new OptionGet(args(0).asInstanceOf[Value[SOption[SAny.type]]])
//      }
//    )
//  )
//
//  registerClassEntry(classOf[OptionGetOrElse[_]],
//    constructors = Array(
//      new SRConstructor[Any](Array(classOf[Value[_]], classOf[Value[_]])) {
//        override def newInstance(args: AnyRef*): Any =
//          new OptionGetOrElse(args(0).asInstanceOf[Value[SOption[SAny.type]]], args(1).asInstanceOf[Value[SAny.type]])
//      }
//    )
//  )
//
//  registerClassEntry(classOf[OptionIsDefined[_]],
//    constructors = Array(
//      new SRConstructor[Any](Array(classOf[Value[_]])) {
//        override def newInstance(args: AnyRef*): Any =
//          new OptionIsDefined(args(0).asInstanceOf[Value[SOption[SAny.type]]])
//      }
//    )
//  )
//
//  registerClassEntry(classOf[SelectField],
//    constructors = Array(
//      new SRConstructor[Any](Array(classOf[Value[_]], classOf[Byte])) {
//        override def newInstance(args: AnyRef*): Any =
//          new SelectField(args(0).asInstanceOf[Value[STuple]], args(1).asInstanceOf[Byte])
//      }
//    )
//  )
//
//  registerClassEntry(classOf[SigmaPropBytes],
//    constructors = Array(
//      new SRConstructor[Any](Array(classOf[Value[_]])) {
//        override def newInstance(args: AnyRef*): Any =
//          new SigmaPropBytes(args(0).asInstanceOf[SigmaPropValue])
//      }
//    )
//  )
//
//  registerClassEntry(classOf[SizeOf[_]],
//    constructors = Array(
//      new SRConstructor[Any](Array(classOf[Value[_]])) {
//        override def newInstance(args: AnyRef*): Any =
//          new SizeOf(args(0).asInstanceOf[CollectionValue[SAny.type]])
//      }
//    )
//  )
//
//  registerClassEntry(classOf[Slice[_]],
//    constructors = Array(
//      new SRConstructor[Any](Array(classOf[Value[_]], classOf[Value[_]], classOf[Value[_]])) {
//        override def newInstance(args: AnyRef*): Any =
//          new Slice(args(0).asInstanceOf[CollectionValue[SAny.type]],
//            args(1).asInstanceOf[IntValue], args(2).asInstanceOf[IntValue])
//      }
//    )
//  )
}
