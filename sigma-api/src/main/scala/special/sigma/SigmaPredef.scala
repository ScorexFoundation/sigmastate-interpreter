package special.sigma

import org.bouncycastle.math.ec.ECPoint

import scala.reflect.ClassTag

object SigmaPredef {
  def dataSize[T](v: T): Long = v match {
    case _: Boolean => 1
    case _: Byte => 1
    case _: Short => 2
    case _: Int => 4
    case _: Long => 8
//    case b: Box => b.dataSize
    case p: ECPoint => p.getEncoded(true).length
    case _ => sys.error(s"Cannot compute dataSize($v)")
  }
  
  //TODO chack how it can be implemented
//  def sizeOf[T](implicit cT: ClassTag[T]): Long = cT match {
//    case _: ClassTag[Boolean] => 1
//    case _: ClassTag[Byte => 1
//    case _: Short => 2
//    case _: Int => 4
//    case _: Long => 8
//    case b: Box => b.dataSize
//    case p: ECPoint => p.getEncoded(true).length
//  }

}


