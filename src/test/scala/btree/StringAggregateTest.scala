package bigvis
package btree

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class StringAggregateTest extends AnyFlatSpec with Matchers {
  behavior of "StringAggregate"

  it should "aggregate summary" in {
    val dataset: Seq[(Long, String)] = (0 until 4) map {i => (i, "error")}

    val result = StringAggregator.aggregator.fromLeaves(dataset)
    result.count should be (4)
    result.summary should be (Some("error"))
  }

  it should "aggregate summary all different none" in {
    val dataset: Seq[(Long, String)] = (0 until 6) map {i => (i, i.toString)}
    val result = StringAggregator.aggregator.fromLeaves(dataset)
    result.count should be (6)
    result.summary should be (None)
  }

  it should "aggregate summary one different none" in {
    val dataset: Seq[(Long, String)] = (0 until 4) map {i => (i, "error")}
    val dataset2 = dataset :+ (4.toLong, "success")

    val result = StringAggregator.aggregator.fromLeaves(dataset2)
    result.count should be (5)
    result.summary should be (None)
  }

  it should "aggregate summary nodes" in {
    val dataset: Seq[(Long, String)] = (0 until 4) map {i => (i, "error")}
    val result1 = StringAggregator.aggregator.fromLeaves(dataset)
    val dataset2: Seq[(Long, String)] = (0 until 4) map {i => (i, "error")}
    val result2 = StringAggregator.aggregator.fromLeaves(dataset2)
    val compdataset: Seq[((Long, Long), StringAggregate)] = Seq(((0, 0), result1), ((0, 1), result2))

    val result = StringAggregator.aggregator.fromNodes(compdataset)
    result.count should be (8)
    result.summary should be (Some("error"))
  }

  it should "aggregate summary nodes one node different" in {
    val dataset: Seq[(Long, String)] = (0 until 4) map {i => (i, "error")}
    val result1 = StringAggregator.aggregator.fromLeaves(dataset)
    val dataset2: Seq[(Long, String)] = (0 until 4) map {i => (i, "success")}
    val result2 = StringAggregator.aggregator.fromLeaves(dataset2)
    val compdataset: Seq[((Long, Long), StringAggregate)] = Seq(((0, 0), result1), ((0, 1), result2))

    val result = StringAggregator.aggregator.fromNodes(compdataset)
    result.count should be (8)
    result.summary should be (None)
  }

  it should "aggregate summary nodes one different none" in {
    val dataset: Seq[(Long, String)] = (0 until 4) map {i => (i, "error")}
    val result1 = StringAggregator.aggregator.fromLeaves(dataset)
    val dataset2: Seq[(Long, String)] = (0 until 3) map {i => (i, "error")}
    val dataset3 = dataset2 :+ (3.toLong, "success")
    val result2 = StringAggregator.aggregator.fromLeaves(dataset3)
    val compdataset: Seq[((Long, Long), StringAggregate)] = Seq(((0, 0), result1), ((0, 1), result2))


    val result = StringAggregator.aggregator.fromNodes(compdataset)
    result.count should be (8)
    result.summary should be (None)
  }


}
