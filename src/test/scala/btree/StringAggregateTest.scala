package bigvis
package btree

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class StringAggregateTest extends AnyFlatSpec with Matchers {
  behavior of "StringAggregate"

  it should "aggregate summary" in {
    val dataset = (0L until 4L) map {i => (i, "error")}

    val result = StringAggregator.aggregator.fromLeaves(dataset)
    result.count should be (4)
    result.summary should be (Some("error"))
  }

  it should "aggregate summary all different none" in {
    val dataset = (0L until 6L) map {i => (i, i.toString)}
    val result = StringAggregator.aggregator.fromLeaves(dataset)
    result.count should be (6)
    result.summary should be (None)
  }

  it should "aggregate summary one different none" in {
    val dataset = (0L until 4L) map {i => (i, "error")}
    val dataset2 = dataset :+ (4L, "success")

    val result = StringAggregator.aggregator.fromLeaves(dataset2)
    result.count should be (5)
    result.summary should be (None)
  }

  it should "aggregate summary nodes" in {
    val dataset = (0L until 4L) map {i => (i, "error")}
    val result1 = StringAggregator.aggregator.fromLeaves(dataset)
    val dataset2 = (4L until 8L) map {i => (i, "error")}
    val result2 = StringAggregator.aggregator.fromLeaves(dataset2)
    val compdataset = Seq(((0L, 3L), result1), ((4L, 7L), result2))

    val result = StringAggregator.aggregator.fromNodes(compdataset)
    result.count should be (8)
    result.summary should be (Some("error"))
  }

  it should "aggregate summary nodes one node different" in {
    val dataset = (0L until 4L) map {i => (i, "error")}
    val result1 = StringAggregator.aggregator.fromLeaves(dataset)
    val dataset2 = (4L until 8L) map {i => (i, "success")}
    val result2 = StringAggregator.aggregator.fromLeaves(dataset2)
    val compdataset = Seq(((0L, 3L), result1), ((4L, 7L), result2))

    val result = StringAggregator.aggregator.fromNodes(compdataset)
    result.count should be (8)
    result.summary should be (None)
  }

  it should "aggregate summary nodes one different none" in {
    val dataset = (0L until 4L) map {i => (i, "error")}
    val result1 = StringAggregator.aggregator.fromLeaves(dataset)
    val dataset2 = ((4L until 7L) map {i => (i, "error")}) :+ (7L, "success")
    val result2 = StringAggregator.aggregator.fromLeaves(dataset2)
    val compdataset = Seq(((0L, 3L), result1), ((4L, 7L), result2))


    val result = StringAggregator.aggregator.fromNodes(compdataset)
    result.count should be (8)
    result.summary should be (None)
  }


}
