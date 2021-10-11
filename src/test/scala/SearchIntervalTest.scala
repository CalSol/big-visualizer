package bigvis

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers


class SearchIntervalTest extends AnyFlatSpec with Matchers {
  behavior of "SearchInterval"

  it should "return contained interval" in {
    SearchInterval(Seq((0, 2), (9, 11), (15, 20)), 10) should equal(  // simple, in center
      Some(SearchInterval.ContainedIn(1))
    )
    SearchInterval(Seq((0, 2), (9, 11), (15, 20)), 11) should equal(  // at edge, in center
      Some(SearchInterval.ContainedIn(1))
    )
    SearchInterval(Seq((0, 2), (9, 11), (15, 20)), 1) should equal(  // first element
      Some(SearchInterval.ContainedIn(0))
    )
    SearchInterval(Seq((0, 2), (9, 11), (15, 20)), 19) should equal(  // last element
      Some(SearchInterval.ContainedIn(2))
    )
  }

  it should "return closest before" in {
    SearchInterval(Seq((0, 2), (9, 11), (15, 20)), 21) should equal(
      Some(SearchInterval.NearestBefore(2, 1))
    )
    SearchInterval(Seq((0, 2), (9, 11), (15, 20)), 1000) should equal(
      Some(SearchInterval.NearestBefore(2, 1000 - 20))
    )
    SearchInterval(Seq((0, 2), (9, 11), (15, 20)), 12) should equal(
      Some(SearchInterval.NearestBefore(1, 1))
    )
    SearchInterval(Seq((0, 2), (9, 11), (15, 20)), 4) should equal(
      Some(SearchInterval.NearestBefore(0, 2))
    )
  }

  it should "return closest after" in {
    SearchInterval(Seq((0, 2), (9, 11), (15, 20)), -1) should equal(
      Some(SearchInterval.NearestAfter(0, 1))
    )
    SearchInterval(Seq((0, 2), (9, 11), (15, 20)), -1000) should equal(
      Some(SearchInterval.NearestAfter(0, 1000))
    )
    SearchInterval(Seq((0, 2), (9, 11), (15, 20)), 6) should equal(
      Some(SearchInterval.NearestAfter(1, 3))
    )
    SearchInterval(Seq((0, 2), (9, 11), (15, 20)), 14) should equal(
      Some(SearchInterval.NearestAfter(2, 1))
    )
  }
}
