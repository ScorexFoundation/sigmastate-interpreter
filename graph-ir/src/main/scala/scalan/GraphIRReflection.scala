package scalan

import scalan.primitives.Thunks
import scalan.reflection.CommonReflection.registerClassEntry
import scalan.reflection.{SRConstructor, mkMethod, SRMethod, mkConstructor}
import special.collection.Colls
import special.sigma.SigmaDsl
import wrappers.scala.WOptions

object GraphIRReflection {

  { val clazz = classOf[wrappers.scala.WOptions#WOption[_]]
    val ctx = null.asInstanceOf[scalan.Library] // ok! type level only
    registerClassEntry(clazz,
      methods = Map(
        mkMethod(clazz, "filter", Array[Class[_]](classOf[Base#Ref[_]])) { (obj, args) =>
          obj.asInstanceOf[ctx.WOption[Any]].filter(args(0).asInstanceOf[ctx.Ref[Any => Boolean]])
        },
        mkMethod(clazz, "get", Array[Class[_]]()) { (obj, args) =>
          obj.asInstanceOf[ctx.WOption[_]].get
        },
        mkMethod(clazz, "isDefined", Array[Class[_]]()) { (obj, args) =>
          obj.asInstanceOf[ctx.WOption[_]].isDefined
        },
        mkMethod(clazz, "getOrElse", Array[Class[_]](classOf[Base#Ref[_]])) { (obj, args) =>
          obj.asInstanceOf[ctx.WOption[Any]].getOrElse(args(0).asInstanceOf[ctx.Ref[ctx.Thunk[Any]]])
        },
        mkMethod(clazz, "map", Array[Class[_]](classOf[Base#Ref[_]])) { (obj, args) =>
          obj.asInstanceOf[ctx.WOption[Any]].map(args(0).asInstanceOf[ctx.Ref[Any => Any]])
        }
      )
    )
  }

  //  registerClassOnly(classOf[Colls#PairColl[_, _]])
//
  registerClassEntry(classOf[TypeDescs#FuncElem[_,_]],
    constructors = Array(
      mkConstructor(Array(classOf[Scalan], classOf[TypeDescs#Elem[_]], classOf[TypeDescs#Elem[_]])) { args =>
        val ctx = args(0).asInstanceOf[Scalan]
        new ctx.FuncElem(args(1).asInstanceOf[ctx.Elem[_]], args(2).asInstanceOf[ctx.Elem[_]])
      }
    )
  )

  registerClassEntry(classOf[TypeDescs#PairElem[_,_]],
    constructors = Array(
      mkConstructor(Array(classOf[Scalan], classOf[TypeDescs#Elem[_]], classOf[TypeDescs#Elem[_]])) { args =>
        val ctx = args(0).asInstanceOf[Scalan]
        new ctx.PairElem(args(1).asInstanceOf[ctx.Elem[_]], args(2).asInstanceOf[ctx.Elem[_]])
      }
    )
  )

  registerClassEntry(classOf[Thunks#ThunkElem[_]],
    constructors = Array(
      mkConstructor(Array(classOf[Scalan], classOf[TypeDescs#Elem[_]])) { args =>
        val ctx = args(0).asInstanceOf[Scalan]
        new ctx.ThunkElem(args(1).asInstanceOf[ctx.Elem[_]])
      }
    )
  )


//  { val ctx = null.asInstanceOf[scalan.Library] // ok! type level only
//    val clazz = classOf[ctx.Coll.CollElem[_, _]]
//    registerClassEntry(clazz,
//      constructors = Array(
//        new SRConstructor[Any](Array(clazz.getDeclaringClass, classOf[TypeDescs#Elem[_]])) {
//          override def newInstance(args: AnyRef*): Any = {
//            val cake = args(0).asInstanceOf[ctx.Coll.type]
//            new cake.CollElem()(args(1).asInstanceOf[ctx.Elem[_]])
//          }
//        }
//      )
//    )
//  }

//  registerClassOnly(classOf[special.sigma.SigmaDsl#AnyValue])
//

  { val clazz = classOf[special.sigma.SigmaDsl#SigmaProp]
    val ctx = null.asInstanceOf[SigmaDsl] // ok! type level only
    registerClassEntry(clazz,
      methods = Map(
        mkMethod(clazz, "$bar$bar", Array[Class[_]](classOf[Base#Ref[_]])) { (obj, args) =>
          obj.asInstanceOf[ctx.SigmaProp].$bar$bar(args(0).asInstanceOf[ctx.Ref[ctx.SigmaProp]])
        },
        mkMethod(clazz, "isValid", Array[Class[_]]()) { (obj, args) =>
          obj.asInstanceOf[ctx.SigmaProp].isValid
        },
        mkMethod(clazz, "propBytes", Array[Class[_]]()) { (obj, args) =>
          obj.asInstanceOf[ctx.SigmaProp].propBytes
        },
        mkMethod(clazz, "$amp$amp", Array[Class[_]](classOf[Base#Ref[_]])) { (obj, args) =>
          obj.asInstanceOf[ctx.SigmaProp].$amp$amp(args(0).asInstanceOf[ctx.Ref[ctx.SigmaProp]])
        }
      )
    )
  }

  { val clazz = classOf[SigmaDsl#BigInt]
    val ctx = null.asInstanceOf[SigmaDsl] // ok! type level only
    registerClassEntry(clazz,
      methods = Map(
        mkMethod(clazz, "add", Array[Class[_]](classOf[Base#Ref[_]])) { (obj, args) =>
          obj.asInstanceOf[ctx.BigInt].add(args(0).asInstanceOf[ctx.Ref[ctx.BigInt]])
        },
        mkMethod(clazz, "max", Array[Class[_]](classOf[Base#Ref[_]])) { (obj, args) =>
          obj.asInstanceOf[ctx.BigInt].max(args(0).asInstanceOf[ctx.Ref[ctx.BigInt]])
        },
        mkMethod(clazz, "min", Array[Class[_]](classOf[Base#Ref[_]])) { (obj, args) =>
          obj.asInstanceOf[ctx.BigInt].min(args(0).asInstanceOf[ctx.Ref[ctx.BigInt]])
        },
        mkMethod(clazz, "subtract", Array[Class[_]](classOf[Base#Ref[_]])) { (obj, args) =>
          obj.asInstanceOf[ctx.BigInt].subtract(args(0).asInstanceOf[ctx.Ref[ctx.BigInt]])
        },
        mkMethod(clazz, "multiply", Array[Class[_]](classOf[Base#Ref[_]])) { (obj, args) =>
          obj.asInstanceOf[ctx.BigInt].multiply(args(0).asInstanceOf[ctx.Ref[ctx.BigInt]])
        },
        mkMethod(clazz, "mod", Array[Class[_]](classOf[Base#Ref[_]])) { (obj, args) =>
          obj.asInstanceOf[ctx.BigInt].mod(args(0).asInstanceOf[ctx.Ref[ctx.BigInt]])
        },
        mkMethod(clazz, "divide", Array[Class[_]](classOf[Base#Ref[_]])) { (obj, args) =>
          obj.asInstanceOf[ctx.BigInt].divide(args(0).asInstanceOf[ctx.Ref[ctx.BigInt]])
        }
      )
    )
  }

  { val clazz = classOf[Colls#CollBuilder]
    val ctx = null.asInstanceOf[Library] // ok! type level only
    registerClassEntry(clazz,
      methods = Map(
        mkMethod(clazz, "xor", Array[Class[_]](classOf[Base#Ref[_]], classOf[Base#Ref[_]])) { (obj, args) =>
          obj.asInstanceOf[ctx.CollBuilder].xor(
            args(0).asInstanceOf[ctx.Ref[ctx.Coll[Byte]]],
            args(1).asInstanceOf[ctx.Ref[ctx.Coll[Byte]]])
        },
        mkMethod(clazz, "fromItems", Array[Class[_]](classOf[Seq[_]], classOf[TypeDescs#Elem[_]])) { (obj, args) =>
          obj.asInstanceOf[ctx.CollBuilder].fromItems[Any](
            args(0).asInstanceOf[Seq[ctx.Ref[Any]]]:_*)(args(1).asInstanceOf[ctx.Elem[Any]])
        },
        mkMethod(clazz, "replicate", Array[Class[_]](classOf[Base#Ref[_]], classOf[Base#Ref[_]])) { (obj, args) =>
          obj.asInstanceOf[ctx.CollBuilder].replicate(args(0).asInstanceOf[ctx.Ref[Int]],
            args(1).asInstanceOf[ctx.Ref[Any]])
        }
      )
    )
  }

  {
    val clazz = classOf[Colls#Coll[_]]
    val ctx = null.asInstanceOf[Library] // ok! type level only
    registerClassEntry(clazz,
      methods = Map(
        mkMethod(clazz, "append", Array[Class[_]](classOf[Base#Ref[_]])) { (obj, args) =>
          obj.asInstanceOf[ctx.Coll[Any]].append(args(0).asInstanceOf[ctx.Ref[ctx.Coll[Any]]])
        },
        mkMethod(clazz, "forall", Array[Class[_]](classOf[Base#Ref[_]])) { (obj, args) =>
          obj.asInstanceOf[ctx.Coll[Any]].forall(args(0).asInstanceOf[ctx.Ref[Any => Boolean]])
        },
        mkMethod(clazz, "length", Array[Class[_]]()) { (obj, args) =>
          obj.asInstanceOf[ctx.Coll[_]].length
        },
        mkMethod(clazz, "updateMany", Array[Class[_]](classOf[Base#Ref[_]], classOf[Base#Ref[_]])) { (obj, args) =>
          obj.asInstanceOf[ctx.Coll[Any]].updateMany(
            args(0).asInstanceOf[ctx.Ref[ctx.Coll[Int]]],
            args(1).asInstanceOf[ctx.Ref[ctx.Coll[Any]]])
        },
        mkMethod(clazz, "updated", Array[Class[_]](classOf[Base#Ref[_]], classOf[Base#Ref[_]])) { (obj, args) =>
          obj.asInstanceOf[ctx.Coll[Any]].updated(
            args(0).asInstanceOf[ctx.Ref[Int]], args(1).asInstanceOf[ctx.Ref[Any]])
        },
        mkMethod(clazz, "patch", Array[Class[_]](classOf[Base#Ref[_]], classOf[Base#Ref[_]], classOf[Base#Ref[_]])) { (obj, args) =>
          obj.asInstanceOf[ctx.Coll[Any]].patch(args(0).asInstanceOf[ctx.Ref[Int]],
            args(1).asInstanceOf[ctx.Ref[ctx.Coll[Any]]],
            args(2).asInstanceOf[ctx.Ref[Int]])
        },
        mkMethod(clazz, "foldLeft", Array[Class[_]](classOf[Base#Ref[_]], classOf[Base#Ref[_]])) { (obj, args) =>
          obj.asInstanceOf[ctx.Coll[Any]].foldLeft(args(0).asInstanceOf[ctx.Ref[Any]],
            args(1).asInstanceOf[ctx.Ref[((Any, Any)) => Any]])
        },
        mkMethod(clazz, "flatMap", Array[Class[_]](classOf[Base#Ref[_]])) { (obj, args) =>
          obj.asInstanceOf[ctx.Coll[Any]].flatMap(args(0).asInstanceOf[ctx.Ref[Any => ctx.Coll[Any]]])
        },
        mkMethod(clazz, "filter", Array[Class[_]](classOf[Base#Ref[_]])) { (obj, args) =>
          obj.asInstanceOf[ctx.Coll[Any]].filter(args(0).asInstanceOf[ctx.Ref[Any => Boolean]])
        },
        mkMethod(clazz, "slice", Array[Class[_]](classOf[Base#Ref[_]], classOf[Base#Ref[_]])) { (obj, args) =>
          obj.asInstanceOf[ctx.Coll[Any]].slice(args(0).asInstanceOf[ctx.Ref[Int]],
            args(1).asInstanceOf[ctx.Ref[Int]])
        },
        mkMethod(clazz, "indices", Array[Class[_]]()) { (obj, args) =>
          obj.asInstanceOf[ctx.Coll[_]].indices
        },
        mkMethod(clazz, "zip", Array[Class[_]](classOf[Base#Ref[_]])) { (obj, args) =>
          obj.asInstanceOf[ctx.Coll[Any]].zip(args(0).asInstanceOf[ctx.Ref[ctx.Coll[Any]]])
        },
        mkMethod(clazz, "map", Array[Class[_]](classOf[Base#Ref[_]])) { (obj, args) =>
          obj.asInstanceOf[ctx.Coll[Any]].map(args(0).asInstanceOf[ctx.Ref[Any => Any]])
        },
        mkMethod(clazz, "getOrElse", Array[Class[_]](classOf[Base#Ref[_]], classOf[Base#Ref[_]])) { (obj, args) =>
          obj.asInstanceOf[ctx.Coll[Any]].getOrElse(args(0).asInstanceOf[ctx.Ref[Int]], args(1).asInstanceOf[ctx.Ref[Any]])
        },
        mkMethod(clazz, "indexOf", Array[Class[_]](classOf[Base#Ref[_]], classOf[Base#Ref[_]])) { (obj, args) =>
          obj.asInstanceOf[ctx.Coll[Any]].indexOf(args(0).asInstanceOf[ctx.Ref[Any]], args(1).asInstanceOf[ctx.Ref[Int]])
        },
        mkMethod(clazz, "apply", Array[Class[_]](classOf[Base#Ref[_]])) { (obj, args) =>
          obj.asInstanceOf[ctx.Coll[_]].apply(args(0).asInstanceOf[ctx.Ref[Int]])
        },
        mkMethod(clazz, "exists", Array[Class[_]](classOf[Base#Ref[_]])) { (obj, args) =>
          obj.asInstanceOf[ctx.Coll[Any]].exists(args(0).asInstanceOf[ctx.Ref[Any => Boolean]])
        }
      )
    )
  }
}
