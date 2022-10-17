package special

import scalan.RType
import scalan.reflection.CommonReflection.registerClassEntry
import scalan.reflection._
import special.collection.Coll
import special.sigma.SigmaProp

object CoreLibReflection {
  {
    val clazz = classOf[SigmaProp]
    registerClassEntry(clazz,
      methods = Map(
        mkMethod(clazz, "$bar$bar", Array[Class[_]](classOf[SigmaProp])) { (obj, args) =>
          obj.asInstanceOf[SigmaProp].$bar$bar(args(0).asInstanceOf[SigmaProp])
        },
        mkMethod(clazz, "isValid", Array[Class[_]]()) { (obj, args) =>
          obj.asInstanceOf[SigmaProp].isValid
        },
        mkMethod(clazz, "propBytes", Array[Class[_]]()) { (obj, args) =>
          obj.asInstanceOf[SigmaProp].propBytes
        },
        mkMethod(clazz, "$amp$amp", Array[Class[_]](classOf[SigmaProp])) { (obj, args) =>
          obj.asInstanceOf[SigmaProp].$amp$amp(args(0).asInstanceOf[SigmaProp])
        }
      )
    )
  }


  //  { val clazz = classOf[special.collection.Coll[_]]
//    registerClassEntry(clazz,
//      methods = Map(
//      {
//        val paramTypes: Seq[Class[_]] = Array(classOf[Int], classOf[java.lang.Object])
//        ("updated", paramTypes) ->
//          new SRMethod(clazz, "updated", paramTypes) {
//            override def invoke(obj: Any, args: AnyRef*): AnyRef = obj match {
//              case obj: Coll[a] =>
//                obj.updated(args(0).asInstanceOf[Int], args(1).asInstanceOf[a])
//            }
//          }
//      },
//      {
//        val paramTypes: Seq[Class[_]] = Array(classOf[java.lang.Object], classOf[Int])
//        ("indexOf", paramTypes) ->
//          new SRMethod(clazz, "indexOf", paramTypes) {
//            override def invoke(obj: Any, args: AnyRef*): AnyRef = obj match {
//              case obj: Coll[a] =>
//                obj.indexOf(args(0).asInstanceOf[a], args(1).asInstanceOf[Int]).asInstanceOf[AnyRef]
//            }
//          }
//      },
//      {
//        val paramTypes: Seq[Class[_]] = Array(classOf[Function1[_, _]], classOf[RType[_]])
//        ("flatMap", paramTypes) ->
//          new SRMethod(clazz, "flatMap", paramTypes) {
//            override def invoke(obj: Any, args: AnyRef*): AnyRef = obj match {
//              case obj: Coll[a] =>
//                obj.flatMap(args(0).asInstanceOf[a => Coll[Any]])(args(1).asInstanceOf[RType[Any]])
//            }
//          }
//      }
//      )
//    )
//  }
}
