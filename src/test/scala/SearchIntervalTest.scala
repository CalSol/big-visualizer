package bigvis

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers


class SearchIntervalTest extends AnyFlatSpec with Matchers {
  behavior of "SearchInterval"

  it should "return contained interval" in {
    SearchInterval(Seq((0, 2), (9, 11), (15, 20)), 10, 0) should equal(  // simple, in center
      Some(SearchInterval.ContainedIn(1))
    )
    SearchInterval(Seq((0, 2), (9, 11), (15, 20)), 11, 0) should equal(  // at edge, in center
      Some(SearchInterval.ContainedIn(1))
    )
    SearchInterval(Seq((0, 2), (9, 11), (15, 20)), 1, 0) should equal(  // first element
      Some(SearchInterval.ContainedIn(0))
    )
    SearchInterval(Seq((0, 2), (9, 11), (15, 20)), 19, 0) should equal(  // last element
      Some(SearchInterval.ContainedIn(2))
    )
  }

  it should "return closest before" in {
    SearchInterval(Seq((0, 2), (9, 11), (15, 20)), 21, Int.MaxValue) should equal(
      Some(SearchInterval.NearestBefore(2, 1))
    )
    SearchInterval(Seq((0, 2), (9, 11), (15, 20)), 1000, Int.MaxValue) should equal(
      Some(SearchInterval.NearestBefore(2, 1000 - 20))
    )
    SearchInterval(Seq((0, 2), (9, 11), (15, 20)), 12, Int.MaxValue) should equal(
      Some(SearchInterval.NearestBefore(1, 1))
    )
    SearchInterval(Seq((0, 2), (9, 11), (15, 20)), 4, Int.MaxValue) should equal(
      Some(SearchInterval.NearestBefore(0, 2))
    )
  }

  it should "return closest after" in {
    SearchInterval(Seq((0, 2), (9, 11), (15, 20)), -1, Int.MaxValue) should equal(
      Some(SearchInterval.NearestAfter(0, 1))
    )
    SearchInterval(Seq((0, 2), (9, 11), (15, 20)), -1000, Int.MaxValue) should equal(
      Some(SearchInterval.NearestAfter(0, 1000))
    )
    SearchInterval(Seq((0, 2), (9, 11), (15, 20)), 6, Int.MaxValue) should equal(
      Some(SearchInterval.NearestAfter(1, 3))
    )
    SearchInterval(Seq((0, 2), (9, 11), (15, 20)), 14, Int.MaxValue) should equal(
      Some(SearchInterval.NearestAfter(2, 1))
    )
  }

  it should "filter before and after by tolerance" in {
    SearchInterval(Seq((0, 2), (9, 11), (15, 20)), 1000, 1000) should equal(
      Some(SearchInterval.NearestBefore(2, 1000 - 20))
    )
    SearchInterval(Seq((0, 2), (9, 11), (15, 20)), -1000, 1000) should equal(
      Some(SearchInterval.NearestAfter(0, 1000))
    )
    SearchInterval(Seq((0, 2), (9, 11), (15, 20)), 1000, 979) should equal(
      None
    )
    SearchInterval(Seq((0, 2), (9, 11), (15, 20)), -1000, 999) should equal(
      None
    )
  }
}
