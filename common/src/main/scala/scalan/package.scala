import scala.reflect.ClassTag

package object scalan {

  /** Allows implicit resolution to find appropriate instance of ClassTag in
    * the scope where RType is implicitly available. */
  implicit def rtypeToClassTag[A](implicit t: RType[A]): ClassTag[A] = t.classTag
  
}
