package bigvis

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

case class FloatAggregate(
  min: Float,
  max: Float,
  count: Long,
  sum: Float,
) {
  def mean: Float = sum / count
}

object FloatAggregate {
  val aggregator = new BTreeAggregator[FloatAggregate, Float] {
    override def fromLeaves(data: Seq[(BTree.TimestampType, Float)]): FloatAggregate = {
      val allData = data.map(_._2)
      FloatAggregate(
        allData.min, allData.max,
        allData.length, allData.sum)
    }

    override def fromNodes(data: Seq[((BTree.TimestampType, BTree.TimestampType), FloatAggregate)]): FloatAggregate = {
      val allNodes = data.map(_._2)
      FloatAggregate(
        allNodes.map(_.min).min, allNodes.map(_.max).max,
        allNodes.map(_.count).sum, allNodes.map(_.sum).sum
      )
    }
  }
}


class BTreeTest extends AnyFlatSpec with Matchers {
  behavior of "BTree"

  it should "push nodeSize items" in {
    val dataset: Seq[(Long, Float)] = (0 until 4) map {i => (i, i)}

    val tree = new BTree(FloatAggregate.aggregator, 4)
    tree.appendAll(dataset)
    tree.toSeq should be(dataset)
    tree.maxDepth should be(1)
  }

  it should "push nodeSize + 1 items" in {
    val dataset: Seq[(Long, Float)] = (0 until 5) map {i => (i, i)}

    val tree = new BTree(FloatAggregate.aggregator, 4)
    tree.appendAll(dataset)
    tree.toSeq should be(dataset)
    tree.maxDepth should be(2)
  }

  it should "push a full two-level tree worth of items" in {
    // leaves would be 2, 2, 2, 4
    val dataset: Seq[(Long, Float)] = (0 until 10) map {i => (i, i)}

    val tree = new BTree(FloatAggregate.aggregator, 4)
    tree.appendAll(dataset)
    tree.toSeq should be(dataset)
    tree.maxDepth should be(2)

    tree.appendAll(Seq((100, 100)))
    tree.maxDepth should be(3)
  }

  it should "push a full three-level tree worth of items" in {
    // leaves would be 2 (2, 2), 2 (2, 2), 2 (2, 2), 4 (2, 2, 2, 4)
    val dataset: Seq[(Long, Float)] = (0 until 22) map {i => (i, i)}

    val tree = new BTree(FloatAggregate.aggregator, 4)
    tree.appendAll(dataset)
    tree.toSeq should be(dataset)
    tree.maxDepth should be(3)

    tree.appendAll(Seq((100, 100)))
    tree.maxDepth should be(4)
  }
}
