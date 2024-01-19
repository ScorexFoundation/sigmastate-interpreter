package sigma.interpreter

import sigma.ast.{EvaluatedValue, SType}

/**
  * Map data structure with traversal ordering corresponding to used in default scala.collection.Map implementation
  * as of Scala 2.12.18. Made in order to make order of ContextExtension variables independent of possible Scala SDK
  * changes and to provide a simple data structure for translating into other programming languages.
  */
class SigmaMap(private val sparseValues: Array[EvaluatedValue[_ <: SType]],
               val maxKey: Byte,
               override val size: Int) extends Map[Byte, EvaluatedValue[_ <: SType]] {

  override def isEmpty: Boolean = maxKey == -1

  override def removed(key: Byte): Map[Byte, EvaluatedValue[_ <: SType]] = {
    sparseValues.update(key, null)
    if (key == maxKey) {
      (maxKey.to(0, -1)).foreach { idx =>
        if (sparseValues(idx) != null) {
          return new SigmaMap(sparseValues, idx.toByte, size - 1)
        }
      }
      new SigmaMap(sparseValues, -1, 0)
    } else {
      new SigmaMap(sparseValues, maxKey, size - 1)
    }
  }

  override def updated[V1 >: EvaluatedValue[_ <: SType]](key: Byte, value: V1): Map[Byte, V1] = {
    val oldValue = sparseValues(key)
    sparseValues.update(key, value.asInstanceOf[EvaluatedValue[_ <: SType]])
    if (oldValue != null) {
      new SigmaMap(sparseValues, maxKey, size)
    } else {
      val newMaxKey = if (key > maxKey) {
        key
      } else {
        maxKey
      }
      new SigmaMap(sparseValues, newMaxKey, size + 1)
    }
  }

  override def get(key: Byte): Option[EvaluatedValue[_ <: SType]] = {
    val res = sparseValues(key).asInstanceOf[EvaluatedValue[_ <: SType]]
    Option(res)
  }

  override def iterator: Iterator[(Byte, EvaluatedValue[_ <: SType])] = {
    if (maxKey == -1) {
      SigmaMap.emptyIterator
    } else if (maxKey <= 4) {
      // keys are coming in ascending order just
      new Iterator[(Byte, EvaluatedValue[_ <: SType])] {
        var lastKey = -1

        override def hasNext: Boolean = lastKey < maxKey

        override def next(): (Byte, EvaluatedValue[_ <: SType]) = {
          var res: EvaluatedValue[_ <: SType] = null
          do {
            lastKey = lastKey + 1
            res = sparseValues(lastKey)
          } while (res == null)
          lastKey.toByte -> res
        }
      }
    } else {
      new Iterator[(Byte, EvaluatedValue[_ <: SType])] {
        var iteratedOver = 0

        override def hasNext: Boolean = ???

        override def next(): (Byte, EvaluatedValue[_ <: SType]) = {
          ???
        }
      }
    }
  }
}

object SigmaMap {

  val emptyIterator = new Iterator[(Byte, EvaluatedValue[_ <: SType])] {
    def hasNext = false
    def next() = throw new NoSuchElementException("next on empty iterator")
  }
  val indices = Array[Byte](69, 101, 0, 88, 115, 5, 120, 10, 56, 42, 24, 37, 25, 52, 14, 110, 125, 20, 46, 93, 57, 78, 29, 106, 121, 84, 61, 89, 116, 1, 74, 6, 60, 117, 85, 102, 28, 38, 70, 21, 33, 92, 65, 97, 9, 53, 109, 124, 77, 96, 13, 41, 73, 105, 2, 32, 34, 45, 64, 17, 22, 44, 59, 118, 27, 71, 12, 54, 49, 86, 113, 81, 76, 7, 39, 98, 103, 91, 66, 108, 3, 80, 35, 112, 123, 48, 63, 18, 95, 50, 67, 16, 127, 31, 11, 72, 43, 99, 87, 104, 40, 26, 55, 114, 23, 8, 75, 119, 58, 82, 36, 30, 51, 19, 107, 4, 126, 79, 94, 47, 15, 68, 62, 90, 111, 122, 83, 100)
  val empty = new SigmaMap(Array.empty[EvaluatedValue[_ <: SType]], -1, 0)
}


