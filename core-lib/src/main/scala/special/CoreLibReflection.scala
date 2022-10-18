package special

import scalan.RType
import scalan.reflection.CommonReflection.registerClassEntry
import scalan.reflection._
import special.collection.{CollBuilder, Coll}
import special.sigma.{SigmaProp, BigInt}
import special.wrappers.OptionWrapSpec

object CoreLibReflection {
  { val clazz = classOf[SigmaProp]
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

  { val clazz = classOf[special.sigma.BigInt]
    val paramTypes = Array[Class[_]](clazz)
    registerClassEntry(clazz,
      methods = Map(
        mkMethod(clazz, "add", paramTypes) { (obj, args) =>
          obj.asInstanceOf[BigInt].add(args(0).asInstanceOf[BigInt])
        },
        mkMethod(clazz, "max", paramTypes) { (obj, args) =>
          obj.asInstanceOf[BigInt].max(args(0).asInstanceOf[BigInt])
        },
        mkMethod(clazz, "min", paramTypes) { (obj, args) =>
          obj.asInstanceOf[BigInt].min(args(0).asInstanceOf[BigInt])
        },
        mkMethod(clazz, "subtract", paramTypes) { (obj, args) =>
          obj.asInstanceOf[BigInt].subtract(args(0).asInstanceOf[BigInt])
        },
        mkMethod(clazz, "multiply", paramTypes) { (obj, args) =>
          obj.asInstanceOf[BigInt].multiply(args(0).asInstanceOf[BigInt])
        },
        mkMethod(clazz, "mod", paramTypes) { (obj, args) =>
          obj.asInstanceOf[BigInt].mod(args(0).asInstanceOf[BigInt])
        },
        mkMethod(clazz, "divide", paramTypes) { (obj, args) =>
          obj.asInstanceOf[BigInt].divide(args(0).asInstanceOf[BigInt])
        }
      )
    )
  }

  { val clazz = classOf[CollBuilder]
    registerClassEntry(clazz,
      methods = Map(
        mkMethod(clazz, "xor", Array[Class[_]](classOf[Coll[_]], classOf[Coll[_]])) { (obj, args) =>
          obj.asInstanceOf[CollBuilder].xor(
            args(0).asInstanceOf[Coll[Byte]],
            args(1).asInstanceOf[Coll[Byte]])
        },
        mkMethod(clazz, "fromItems", Array[Class[_]](classOf[Seq[_]], classOf[RType[_]])) { (obj, args) =>
          obj.asInstanceOf[CollBuilder].fromItems[Any](
            args(0).asInstanceOf[Seq[Any]]: _*)(args(1).asInstanceOf[RType[Any]])
        },
        mkMethod(clazz, "replicate", Array[Class[_]](classOf[Int], classOf[Object], classOf[RType[_]])) { (obj, args) =>
          obj.asInstanceOf[CollBuilder].replicate(args(0).asInstanceOf[Int],
            args(1).asInstanceOf[Any])(args(2).asInstanceOf[RType[Any]])
        }
      )
    )
  }

  { val clazz = classOf[OptionWrapSpec]
    registerClassEntry(clazz,
      methods = Map(
        mkMethod(clazz, "getOrElse", Array[Class[_]](classOf[Option[_]], classOf[Function0[_]])) { (obj, args) =>
          val opt = args(0).asInstanceOf[Option[Any]]
          val defaultFunc = args(1).asInstanceOf[Function0[Any]]
          obj.asInstanceOf[OptionWrapSpec].getOrElse(opt, defaultFunc())
        },
        mkMethod(clazz, "isDefined", Array[Class[_]](classOf[Option[_]])) { (obj, args) =>
          obj.asInstanceOf[OptionWrapSpec].isDefined(args(0).asInstanceOf[Option[_]])
        },
        mkMethod(clazz, "filter", Array[Class[_]](classOf[Option[_]], classOf[Function1[_,_]])) { (obj, args) =>
          obj.asInstanceOf[OptionWrapSpec].filter(
            args(0).asInstanceOf[Option[Any]], args(1).asInstanceOf[Any => Boolean])
        },
        mkMethod(clazz, "map", Array[Class[_]](classOf[Option[_]], classOf[Function1[_,_]])) { (obj, args) =>
          obj.asInstanceOf[OptionWrapSpec].map(
            args(0).asInstanceOf[Option[Any]], args(1).asInstanceOf[Any => Any])
        },
        mkMethod(clazz, "get", Array[Class[_]](classOf[Option[_]])) { (obj, args) =>
          obj.asInstanceOf[OptionWrapSpec].get(args(0).asInstanceOf[Option[_]])
        }
      )
    )
  }

  { val clazz = classOf[Coll[_]]
    registerClassEntry(clazz,
      methods = Map(
        mkMethod(clazz, "updated", Array[Class[_]](classOf[Int], classOf[java.lang.Object])) { (obj, args) =>
          obj.asInstanceOf[Coll[Any]].updated(args(0).asInstanceOf[Int], args(1).asInstanceOf[Any])
        },
        mkMethod(clazz, "flatMap", Array[Class[_]](classOf[Function1[_, _]], classOf[RType[_]])) { (obj, args) =>
          obj.asInstanceOf[Coll[Any]].flatMap(
            args(0).asInstanceOf[Any => Coll[Any]])(args(1).asInstanceOf[RType[Any]])
        },
        mkMethod(clazz, "apply", Array[Class[_]](classOf[Int])) { (obj, args) =>
          obj.asInstanceOf[Coll[_]].apply(args(0).asInstanceOf[Int])
        },
        mkMethod(clazz, "append", Array[Class[_]](classOf[Coll[_]])) { (obj, args) =>
          obj.asInstanceOf[Coll[Any]].append(args(0).asInstanceOf[Coll[Any]])
        },
        mkMethod(clazz, "indexOf", Array[Class[_]](classOf[java.lang.Object], classOf[Int])) { (obj, args) =>
          obj.asInstanceOf[Coll[Any]].indexOf(args(0).asInstanceOf[Any], args(1).asInstanceOf[Int])
        },
        mkMethod(clazz, "map", Array[Class[_]](classOf[Function1[_, _]], classOf[RType[_]])) { (obj, args) =>
          obj.asInstanceOf[Coll[Any]].map(args(0).asInstanceOf[Any => Any])(args(1).asInstanceOf[RType[Any]])
        }
      )
    )
  }
}
