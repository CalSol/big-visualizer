package bigvis
package btree

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers


class BTreeResamplerTest extends AnyFlatSpec with Matchers {
  behavior of "BTreeResampler"

  def aggregateNodeToTuple[AggregatorType <: BTreeAggregator](node: BTreeData[AggregatorType]):
      ((BTree.TimestampType, BTree.TimestampType), AggregatorType#NodeType) = {
    val nodeNode = node.asInstanceOf[BTreeAggregate[AggregatorType]]
    ((nodeNode.minTime, nodeNode.maxTime), nodeNode.nodeData)
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
      ((0, 3), FloatAggregate(0, 20, 4, 50)),
      ((10, 11), FloatAggregate(500, 600, 2, 1100)),
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
      ((1, 1), FloatAggregate(10, 10, 1, 10)),
      ((2, 3), FloatAggregate(20, 30, 2, 50)),
      ((4, 5), FloatAggregate(40, 50, 2, 90)),
      ((6, 6), FloatAggregate(60, 60, 1, 60)),
      ((8, 8), FloatAggregate(80, 80, 1, 80)),
    ))
  }
}
