package bigvis
package util

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers


class TupleArrayTest extends AnyFlatSpec with Matchers {
  behavior of "TupleArray"

  it should "build array and support basic access" in {
    val builder = new TupleArrayBuilder[Int, Int]()
    builder.addOne(0, 1)
    builder.addOne(2, 3)
    val result = builder.result()
    result.toArraySlow should equal(Seq((0, 1), (2, 3)))
    result.length should equal(2)
    result.isEmpty should equal(false)
    result.nonEmpty should equal(true)
    result.head_1 should equal(0)
    result.head_2 should equal(1)
    result.last_1 should equal(2)
    result.last_2 should equal(3)
  }

  it should "work with empty arrays" in {
    val builder = new TupleArrayBuilder[Int, Int]()
    val result = builder.result()
    result.toArraySlow should equal(Seq())
    result.length should equal(0)
    result.isEmpty should equal(true)
    result.nonEmpty should equal(false)
  }

  it should "filter" in {
    val builder = new TupleArrayBuilder[Int, Int]()
    builder.addAll(Seq((0, 1), (2, 3)))
    val result = builder.result().filter { case (v1, v2) => v1 == 0 }
    result.toArraySlow should equal(Seq((0, 1)))
  }

  it should "tail" in {
    val builder = new TupleArrayBuilder[Int, Int]()
    builder.addAll(Seq((0, 1), (2, 3), (4, 5)))
    val tail = builder.result().tail
    tail.length should equal(2)
    tail.toArraySlow should equal(Seq((2, 3), (4, 5)))
    tail.head_1 should equal(2)
    tail.head_2 should equal(3)
    tail.last_1 should equal(4)
    tail.last_2 should equal(5)

    val tail2 = tail.tail
    tail2.length should equal(1)
    tail2.toArraySlow should equal(Seq((4, 5)))
    tail2.head_1 should equal(4)
    tail2.head_2 should equal(5)
    tail2.last_1 should equal(4)
    tail2.last_2 should equal(5)
  }

  it should "splitAt" in {
    val builder = new TupleArrayBuilder[Int, Int]()
    builder.addAll(Seq((0, 1), (2, 3), (4, 5)))
    val result = builder.result()

    val (first, second) = result.splitAt(1)
    first.toArraySlow should equal(Seq((0, 1)))
    first.length should equal(1)
    first.head_1 should equal(0)
    first.head_2 should equal(1)
    first.last_1 should equal(0)
    first.last_2 should equal(1)
    second.toArraySlow should equal(Seq((2, 3), (4, 5)))
    second.length should equal(2)
    second.head_1 should equal(2)
    second.head_2 should equal(3)
    second.last_1 should equal(4)
    second.last_2 should equal(5)

    val (first0, second0) = result.splitAt(0)
    first0.toArraySlow should equal(Seq())
    first0.length should equal(0)
    first0.isEmpty should equal(true)
    second0.toArraySlow should equal(Seq((0, 1), (2, 3), (4, 5)))
    second0.length should equal(3)
    second0.isEmpty should equal(false)
    second0.head_1 should equal(0)
    second0.head_2 should equal(1)
    second0.last_1 should equal(4)
    second0.last_2 should equal(5)

    val (first3, second3) = result.splitAt(3)
    first3.toArraySlow should equal(Seq((0, 1), (2, 3), (4, 5)))
    first3.length should equal(3)
    first3.isEmpty should equal(false)
    first3.head_1 should equal(0)
    first3.head_2 should equal(1)
    first3.last_1 should equal(4)
    first3.last_2 should equal(5)
    second3.toArraySlow should equal(Seq())
    second3.length should equal(0)
    second3.isEmpty should equal(true)
  }

  it should "map" in {
    val builder = new TupleArrayBuilder[Int, Int]()
    builder.addAll(Seq((0, 1), (2, 3), (4, 5)))
    builder.result().map(_ + _) should equal(Seq(1, 5, 9))
  }
}
