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

    tree.appendAll(Seq((dataset.length, dataset.length)))
    tree.maxDepth should be(3)
  }

  it should "push a full three-level tree worth of items" in {
    // leaves would be 2 (2, 2), 2 (2, 2), 2 (2, 2), 4 (2, 2, 2, 4)
    val dataset: Seq[(Long, Float)] = (0 until 22) map {i => (i, i)}

    val tree = new BTree(FloatAggregate.aggregator, 4)
    tree.appendAll(dataset)
    tree.toSeq should be(dataset)
    tree.maxDepth should be(3)

    tree.appendAll(Seq((dataset.length, dataset.length)))
    tree.maxDepth should be(4)
  }

  it should "push a full three-level tree worth of items, with n=16" in {
    // generalizing from above, most nodes are length 8, except the rightmost ones are length 16
    // so we have a typical tree of 16*8*8, plus 8*8 nodes on rightmost second level, plus 8 nodes on rightmost third level
    val dataset: Seq[(Long, Float)] = (0 until (16*8*8 + 8*8 + 8)) map {i => (i, i)}

    val tree = new BTree(FloatAggregate.aggregator, 16)
    tree.appendAll(dataset)
    tree.toSeq should be(dataset)
    tree.maxDepth should be(3)

    tree.appendAll(Seq((dataset.length, dataset.length)))
    tree.maxDepth should be(4)
  }

  // using three levels, leaves would be (2, 2), (2, 2, 2, 4) long
  val dataset: Seq[(Long, Float)] = Seq(
    // the first top-level node is too large
    (0, 0), (1, 1),
    (2, 2), (100, 3),

    // the second top-level node is returned as an aggregate
    (101, 4), (102, 5),
    (103, 6), (104, 7),
    (105, 8), (106, 9),
    (107, 10), (108, 11), (109, 12), (110, 13)
  )
  val datasetTree = new BTree(FloatAggregate.aggregator, 4)
  datasetTree.appendAll(dataset)

  it should "have a correct dataset tree" in {
    datasetTree.toSeq should be(dataset)
    datasetTree.maxDepth should be(3)
  }

  it should "return getNodes mixing leaf and intermediate notes" in {
    val nodes = datasetTree.getNodes(0, 111, 10)
    nodes.length should be(3)
    nodes(0).minTime should be(0)
    nodes(0).maxTime should be(1)
    nodes(0).nodeData.count should be(2)
    nodes(0).nodeData.sum should be(0 + 1)

    nodes(1).minTime should be(2)
    nodes(1).maxTime should be(100)
    nodes(1).nodeData.count should be(2)
    nodes(1).nodeData.sum should be(2 + 3)

    nodes(2).minTime should be(101)
    nodes(2).maxTime should be(110)
    nodes(2).nodeData.count should be(10)
    nodes(2).nodeData.sum should be(4 + 5 + 6 + 7 + 8 + 9 + 10 + 11 + 12 + 13)
  }

  it should "return getNodes filtering by time, cutting off the start" in {
    val nodes = datasetTree.getNodes(3, 102, 10)
    nodes.length should be(2)

    nodes(0).minTime should be(2)
    nodes(0).maxTime should be(100)
    nodes(0).nodeData.count should be(2)
    nodes(0).nodeData.sum should be(2 + 3)

    nodes(1).minTime should be(101)
    nodes(1).maxTime should be(110)
    nodes(1).nodeData.count should be(10)
    nodes(1).nodeData.sum should be(4 + 5 + 6 + 7 + 8 + 9 + 10 + 11 + 12 + 13)
  }

  it should "return getNodes filtering by time, cutting off the end" in {
    val nodes = datasetTree.getNodes(0, 101, 10)
    nodes.length should be(2)

    nodes(0).minTime should be(0)
    nodes(0).maxTime should be(1)
    nodes(0).nodeData.count should be(2)
    nodes(0).nodeData.sum should be(0 + 1)

    nodes(1).minTime should be(2)
    nodes(1).maxTime should be(100)
    nodes(1).nodeData.count should be(2)
    nodes(1).nodeData.sum should be(2 + 3)
  }

  it should "return getNodes filtering by time, cutting off both start and end" in {
    val nodes = datasetTree.getNodes(3, 101, 10)
    nodes.length should be(1)

    nodes(0).minTime should be(2)
    nodes(0).maxTime should be(100)
    nodes(0).nodeData.count should be(2)
    nodes(0).nodeData.sum should be(2 + 3)
  }
}
