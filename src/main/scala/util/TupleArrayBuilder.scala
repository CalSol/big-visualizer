package bigvis
package util

import scala.reflect.ClassTag


/** A data structure that looks like ArrayBuilder[(T1, T2)], but using two ArrayBuilder[T] inside
 * which allows for space- and time- efficient unboxed types with T is primitive.
 */
class TupleArrayBuilder[@specialized(Long) T1, @specialized T2](implicit t1: ClassTag[T1], t2: ClassTag[T2]) {
  val builder1 = Array.newBuilder[T1]
  val builder2 = Array.newBuilder[T2]

  def clear(): Unit = {
    builder1.clear()
    builder2.clear()
  }

  def addOne(elem1: T1, elem2: T2): Unit = {
    builder1.addOne(elem1)
    builder2.addOne(elem2)
  }

  // Returns the TupleArray version of this. Further operations on this ArrayBuilder are undefined
  // except clear().
  def result(): TupleArray[T1, T2] = {
    new TupleArray(builder1.result(), builder2.result())
  }
}


/** A data structure that looks like Array[(T1, T2)], but using two Array[T] inside
 * which allows for space- and time- efficient unboxed types with T is primitive.
 *
 * Probably trades off some memory locality, but if the types were boxed and on the heap
 * then there might not have been much memory locality to begin with.
 */
class TupleArray[@specialized(Long) T1, @specialized T2](array1: Array[T1], array2: Array[T2])
                                                        (implicit t1: ClassTag[T1], t2: ClassTag[T2]) {
  require(array1.length == array2.length)

  // Converts this to an Array[(T1, T2)], which loses any unboxing benefits
  def toArraySlow: Array[(T1, T2)] = {
    array1 zip array2
  }

  def filter[V1, V2](fn: (T1, T2) => Boolean): TupleArray[T1, T2] = {
    // Imperative operations are much faster than something like zip/zipped:
    // https://stackoverflow.com/questions/59598239/why-is-zipped-faster-than-zip-in-scala
    val outputBuilder = new TupleArrayBuilder[T1, T2]()
    var i: Int = 0
    while (i < array1.length) {
      if (fn(array1(i), array2(i))) {
        outputBuilder.addOne(array1(i), array2(i))
      }
      i = i + 1
    }
    outputBuilder.result()
  }
}
