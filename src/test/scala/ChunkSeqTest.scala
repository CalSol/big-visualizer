package bigvis

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ChunkSeqTest extends AnyFlatSpec with Matchers {
  behavior of "ChunkSeq"

  it should "pass through unsplit seq" in {
    ChunkSeq[Int, Int](0 until 4, 0, (data, elem) => (data, false)) should equal(
      Seq(0 until 4)
    )
  }

  it should "split into two" in {
    ChunkSeq[Int, Int](0 until 4, 0, (data, elem) => (0, elem == 3)) should equal(
      Seq(Seq(0, 1, 2), Seq(3))
    )
  }

  it should "split into three" in {
    ChunkSeq[Int, Int](0 until 4, 0, (data, elem) => (0, elem == 1 || elem == 3)) should equal(
      Seq(Seq(0), Seq(1, 2), Seq(3))
    )
  }

  it should "split all" in {
    ChunkSeq[Int, Int](0 until 4, 0, (data, elem) => (0, true)) should equal(
      Seq(Seq(0), Seq(1), Seq(2), Seq(3))
    )
  }

  it should "preserve stateful data" in {
    ChunkSeq[Int, Int](0 until 4, 0, (data, elem) => (data + 1, data == 1)) should equal(
      Seq(Seq(0), Seq(1, 2, 3))
    )
  }
}
