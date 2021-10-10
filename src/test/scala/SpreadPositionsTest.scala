package bigvis

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers


class SpreadPositionsTest extends AnyFlatSpec with Matchers {
  behavior of "SpreadPositions"

  it should "pass through non-overlapping items" in {
    CursorCanvas.spreadPositions(Seq(), 10, 0, 100) should equal(
      Seq()
    )
    CursorCanvas.spreadPositions(Seq(10), 10, 0, 100) should equal(
      Seq(10)
    )
    CursorCanvas.spreadPositions(Seq(0, 10, 20), 10, 0, 100) should equal(
      Seq(0, 10, 20)
    )
  }

  it should "spread overlapping items" in {
    CursorCanvas.spreadPositions(Seq(10, 10), 10, 0, 100) should equal(
      Seq(5, 15)
    )
    CursorCanvas.spreadPositions(Seq(15, 15, 15),10, 0, 100) should equal(
      Seq(5, 15, 25)
    )

    // would propagate downward
    CursorCanvas.spreadPositions(Seq(14, 15, 16),10, 0, 100) should equal(
      Seq(5, 15, 25)
    )
    // would propagate upward
    CursorCanvas.spreadPositions(Seq(5, 15, 25, 25),10, 0, 100) should equal(
      Seq(5, 15, 25, 35)
    )
  }

  it should "clip" in {
    CursorCanvas.spreadPositions(Seq(0, 0), 10, 0, 100) should equal(
      Seq(5, 15)
    )
    CursorCanvas.spreadPositions(Seq(100, 100),10, 0, 100) should equal(
      Seq(85, 95)
    )
  }
}
