package bigvis
package btree

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers


class BTreeResamplerTest extends AnyFlatSpec with Matchers {
  behavior of "BTreeResampler"

  val aggregator = FloatAggregator.aggregator
  type AggregatorType = FloatAggregator

  def aggregateNodeToTuple(node: BTreeData[AggregatorType]):
      ((BTree.TimestampType, BTree.TimestampType), AggregatorType#NodeType, String) = node match {
    case node: BTreeAggregate[AggregatorType] =>
      ((node.minTime, node.maxTime), node.nodeData, "agg")
    case node: BTreeLeaf[AggregatorType] =>
      ((node.point._1, node.point._1), aggregator.fromLeaves(Seq(node.point)), "leaf")
  }

  it should "resample nodes" in {
    val dataset = Seq(
      // combine these two
      new BTreeResampledNode[FloatAggregator](0, 1, FloatAggregate(0, 10, 2, 20)),
      new BTreeResampledNode[FloatAggregator](2, 3, FloatAggregate(10, 20, 2, 30)),
      // do not aggregate this
      new BTreeResampledNode[FloatAggregator](10, 11, FloatAggregate(500, 600, 2, 1100)),
    )

    val resampled = BTreeResampler(FloatAggregator.aggregator, dataset, 4)
    resampled.map(aggregateNodeToTuple) should equal(Seq(
      ((0, 3), FloatAggregate(0, 20, 4, 50), "agg"),
      ((10, 11), FloatAggregate(500, 600, 2, 1100), "agg"),
    ))
  }

  it should "resample leaves" in {
    val dataset = Seq(
      // not combined
      new BTreeLeaf[FloatAggregator]((1, 10)),
      // alignment boundary, combine these two
      new BTreeLeaf[FloatAggregator]((2, 20)),
      new BTreeLeaf[FloatAggregator]((3, 30)),
      // combine these two
      new BTreeLeaf[FloatAggregator]((4, 40)),
      new BTreeLeaf[FloatAggregator]((5, 50)),
      // do not combine, too far apart
      new BTreeLeaf[FloatAggregator]((6, 60)),
      new BTreeLeaf[FloatAggregator]((8, 80)),
    )

    val resampled = BTreeResampler(FloatAggregator.aggregator, dataset, 2)
    resampled.map(aggregateNodeToTuple) should equal(Seq(
      ((1, 1), FloatAggregate(10, 10, 1, 10), "leaf"),
      ((2, 3), FloatAggregate(20, 30, 2, 50), "agg"),
      ((4, 5), FloatAggregate(40, 50, 2, 90), "agg"),
      ((6, 6), FloatAggregate(60, 60, 1, 60), "leaf"),
      ((8, 8), FloatAggregate(80, 80, 1, 80), "leaf"),
    ))
  }

  it should "resample combining nodes and leaves" in {
    val dataset = Seq(
      // merged
      new BTreeLeaf[FloatAggregator]((0, 1)),
      new BTreeResampledNode[FloatAggregator](1, 2, FloatAggregate(10, 20, 2, 30)),
      // not combined - too far apart
      new BTreeResampledNode[FloatAggregator](3, 4, FloatAggregate(30, 40, 2, 70)),
    )

    val resampled = BTreeResampler(FloatAggregator.aggregator, dataset, 4)
    resampled.map(aggregateNodeToTuple) should equal(Seq(
      ((0, 2), FloatAggregate(1, 20, 3, 31), "agg"),
      ((3, 4), FloatAggregate(30, 40, 2, 70), "agg"),
    ))
  }
}
