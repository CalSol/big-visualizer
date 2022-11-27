package bigvis
package util

import scala.reflect.ClassTag


/** A data structure that looks like ArrayBuilder[(T1, T2)], but using two ArrayBuilder[T] inside
 * which allows for space- and time- efficient unboxed types with T is primitive.
 */
class TupleArrayBuilder[@specialized(Long) T1, @specialized T2](implicit t1: ClassTag[T1], t2: ClassTag[T2]) {
  val builder1 = Array.newBuilder[T1]
  val builder2 = Array.newBuilder[T2]

  def length: Int = builder1.length

  def clear(): Unit = {
    builder1.clear()
    builder2.clear()
  }

  def addOne(elem1: T1, elem2: T2): Unit = {
    builder1.addOne(elem1)
    builder2.addOne(elem2)
  }

  def addAll(elems1: Array[T1], elems2: Array[T2]): Unit = {
    builder1.addAll(elems1)
    builder2.addAll(elems2)
  }

  // Alternative (less efficient) version of addAll that uses Seq of Tuples
  def addAll(elems: Seq[(T1, T2)]): Unit = {
    builder1.addAll(elems.map(_._1))
    builder2.addAll(elems.map(_._2))
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
 * Supports fast slicing with an optional starting and ending index.
 *
 * Probably trades off some memory locality, but if the types were boxed and on the heap
 * then there might not have been much memory locality to begin with.
 */
class TupleArray[@specialized(Long) T1, @specialized T2](val array1: Array[T1], val array2: Array[T2],
                                                         val startIndex: Int = 0, endIndex: Int = -1)
                                                        (implicit t1: ClassTag[T1], t2: ClassTag[T2]) {
  require(array1.length == array2.length)
  require(startIndex >= 0 && startIndex <= array1.length)  // if equal length, means empty array
  require((endIndex >= 0 || endIndex == -1) && (endIndex <= array1.length))
  val actualEndIndex: Int = if (endIndex == -1) array1.length else endIndex

  def this()(implicit t1: ClassTag[T1], t2: ClassTag[T2]) = {  // empty array constructor
    this(Array[T1](), Array[T2]())
  }

  // Converts this to an Array[(T1, T2)], which loses any unboxing benefits
  def toArraySlow: Array[(T1, T2)] = {
    array1.slice(startIndex, actualEndIndex) zip array2.slice(startIndex, actualEndIndex)
  }

  // Creates a builder initialized with the data in this array
  def toBuilder: TupleArrayBuilder[T1, T2] = {
    val builder = new TupleArrayBuilder[T1, T2]()
    var i: Int = startIndex
    while (i < actualEndIndex) {
      builder.addOne(array1(i), array2(i))
      i = i + 1
    }
    builder
  }

  def length: Int = actualEndIndex - startIndex
  def isEmpty: Boolean = length == 0
  def nonEmpty: Boolean = length > 0

  // Same as Array[(T1, T2)].head._1 / ._2 but without needing a tuple wrapper
  def head_1: T1 = array1(startIndex)
  def head_2: T2 = array2(startIndex)
  // Same as Array[(T1, T2)].last._1 / ._2 but without needing a tuple wrapper
  def last_1: T1 = array1(actualEndIndex - 1)
  def last_2: T2 = array2(actualEndIndex - 1)

  def tail: TupleArray[T1, T2] = {
    new TupleArray(array1, array2, startIndex + 1, endIndex)
  }

  // Concatenates two arrays. Slow operation.
  def ++(that: TupleArray[T1, T2]): TupleArray[T1, T2] = {
    val builder = toBuilder
    var thatI: Int = that.startIndex
    while (thatI < that.actualEndIndex) {
      builder.addOne(that.array1(thatI), that.array2(thatI))
      thatI = thatI + 1
    }
    builder.result()
  }

  def filter(fn: (T1, T2) => Boolean): TupleArray[T1, T2] = {
    // Imperative operations are much faster than something like zip/zipped:
    // https://stackoverflow.com/questions/59598239/why-is-zipped-faster-than-zip-in-scala
    val outputBuilder = new TupleArrayBuilder[T1, T2]()
    var i: Int = startIndex
    while (i < actualEndIndex) {
      if (fn(array1(i), array2(i))) {
        outputBuilder.addOne(array1(i), array2(i))
      }
      i = i + 1
    }
    outputBuilder.result()
  }

  def map[V1](fn: (T1, T2) => V1)(implicit v1: ClassTag[V1]): Array[V1] = {
    val outputBuilder = Array.newBuilder[V1]
    var i: Int = startIndex
    while (i < actualEndIndex) {
      outputBuilder.addOne(fn(array1(i), array2(i)))
      i = i + 1
    }
    outputBuilder.result()
  }

  def splitAt(index: Int): (TupleArray[T1, T2], TupleArray[T1, T2]) = {
    (new TupleArray(array1, array2, startIndex, startIndex + index),
      new TupleArray(array1, array2, startIndex + index, endIndex))
  }
}
