package bigvis
package util

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers


class TupleArrayTest extends AnyFlatSpec with Matchers {
  behavior of "TupleArray"

  it should "build array" in {
    val builder = new TupleArrayBuilder[Int, Int]()
    builder.addOne(0, 1)
    builder.addOne(2, 3)
    builder.result().toArraySlow should equal(Seq((0, 1), (2, 3)))
  }

  it should "work with empty arrays" in {
    val builder = new TupleArrayBuilder[Int, Int]()
    builder.result().toArraySlow should equal(Seq())
  }

  it should "filter" in {
    val builder = new TupleArrayBuilder[Int, Int]()
    builder.addOne(0, 1)
    builder.addOne(2, 3)
    val result = builder.result().filter { case (v1, v2) => v1 == 0 }
    result.toArraySlow should equal(Seq((0, 1)))
  }
}