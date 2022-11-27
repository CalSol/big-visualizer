package bigvis
package btree

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers


class BTreeTest extends AnyFlatSpec with Matchers {
  behavior of "BTree"

  val leafSize = 4
  val nodeSize = 4

  it should "push nodeSize items" in {
    val dataset: Seq[(Long, Float)] = (0 until 4) map {i => (i, i.toFloat)}

    val tree = new BTree(FloatAggregator.aggregator, leafSize, nodeSize)
    tree.appendAll(dataset)
    tree.toSeqSlow should be(dataset)
    tree.minTime should be(0)
    tree.maxTime should be(3)
    tree.maxDepth should be(1)
    tree.validate() should be (true)
  }

  it should "push nodeSize + 1 items" in {
    val dataset: Seq[(Long, Float)] = (0 until 5) map {i => (i, i.toFloat)}

    val tree = new BTree(FloatAggregator.aggregator, leafSize, nodeSize)
    tree.appendAll(dataset)
    tree.toSeqSlow should be(dataset)
    tree.minTime should be(0)
    tree.maxTime should be(4)
    tree.maxDepth should be(2)
    tree.validate() should be (true)
  }

  it should "push a full two-level tree worth of items" in {
    // leaves would be 2, 2, 2, 4
    val dataset: Seq[(Long, Float)] = (0 until 10) map {i => (i, i.toFloat)}

    val tree = new BTree(FloatAggregator.aggregator, leafSize, nodeSize)
    tree.appendAll(dataset)
    tree.toSeqSlow should be(dataset)
    tree.minTime should be(0)
    tree.maxTime should be(9)
    tree.maxDepth should be(2)
    tree.validate() should be (true)

    tree.appendAll(Seq((dataset.length.toLong, dataset.length.toFloat)))
    tree.minTime should be(0)
    tree.maxTime should be(10)
    tree.maxDepth should be(3)
    tree.validate() should be (true)
  }

  it should "push a full three-level tree worth of items" in {
    // leaves would be 2 (2, 2), 2 (2, 2), 2 (2, 2), 4 (2, 2, 2, 4)
    val dataset: Seq[(Long, Float)] = (0 until 22) map {i => (i, i.toFloat)}

    val tree = new BTree(FloatAggregator.aggregator, leafSize, nodeSize)
    tree.appendAll(dataset)
    tree.toSeqSlow should be(dataset)
    tree.minTime should be(0)
    tree.maxTime should be(21)
    tree.maxDepth should be(3)
    tree.validate() should be (true)

    tree.appendAll(Seq((dataset.length.toLong, dataset.length.toFloat)))
    tree.minTime should be(0)
    tree.maxTime should be(22)
    tree.maxDepth should be(4)
    tree.validate() should be (true)
  }

  it should "push a full three-level tree worth of items, with n=16" in {
    // generalizing from above, most nodes are length 8, except the rightmost ones are length 16
    // so we have a typical tree of 16*8*8, plus 8*8 nodes on rightmost second level, plus 8 nodes on rightmost third level
    val dataset: Seq[(Long, Float)] = (0 until (16*8*8 + 8*8 + 8)) map {i => (i, i.toFloat)}

    val tree = new BTree(FloatAggregator.aggregator, 16, 16)
    tree.appendAll(dataset)
    tree.toSeqSlow should be(dataset)
    tree.maxDepth should be(3)
    tree.validate() should be (true)

    tree.appendAll(Seq((dataset.length.toLong, dataset.length.toFloat)))
    tree.maxDepth should be(4)
    tree.validate() should be (true)
  }

  // using three levels, leaves would be (2, 2), (2, 2, 2, 4) long
  val dataset: Seq[(Long, Float)] = Seq(
    // the first top-level node is too large
    (0, 0), (1, 1),
    (2, 2), (100, 3),

    // the second top-level node is returned as an aggregate
    (101, 4), (102, 5),
    (103, 6), (104, 7),
    (105, 8), (120, 9),
    (121, 10), (122, 11), (123, 12), (124, 13)
  )
  val datasetTree = new BTree(FloatAggregator.aggregator, leafSize, nodeSize)
  datasetTree.appendAll(dataset)

  it should "have a correct dataset tree" in {
    datasetTree.toSeqSlow should be(dataset)
    datasetTree.maxDepth should be(3)
    datasetTree.validate() should be (true)
  }

  it should "return getNodes mixing leaf and intermediate notes" in {
    val nodes = datasetTree.getData(0, 111, 50)
    nodes.length should be(4)
    val node0 = nodes(0).asInstanceOf[BTreeNode[FloatAggregator]]
    node0.minTime should be(0)
    node0.maxTime should be(1)
    node0.nodeData.count should be(2)
    node0.nodeData.sum should be(0 + 1)

    val node1 = nodes(1).asInstanceOf[BTreeLeaf[FloatAggregator]]
    node1.point should be((2, 2))
    val node2 = nodes(2).asInstanceOf[BTreeLeaf[FloatAggregator]]
    node2.point should be((100, 3))

    val node3 = nodes(3).asInstanceOf[BTreeNode[FloatAggregator]]
    node3.minTime should be(101)
    node3.maxTime should be(124)
    node3.nodeData.count should be(10)
    node3.nodeData.sum should be(4 + 5 + 6 + 7 + 8 + 9 + 10 + 11 + 12 + 13)
  }

  it should "return getNodes filtering by time, cutting off the start" in {
    val nodes = datasetTree.getData(3, 102, 50)
    nodes.length should be(2)

    val node1 = nodes(0).asInstanceOf[BTreeLeaf[FloatAggregator]]
    node1.point should be((100, 3))

    val node2 = nodes(1).asInstanceOf[BTreeNode[FloatAggregator]]
    node2.minTime should be(101)
    node2.maxTime should be(124)
    node2.nodeData.count should be(10)
    node2.nodeData.sum should be(4 + 5 + 6 + 7 + 8 + 9 + 10 + 11 + 12 + 13)
  }

  it should "return getNodes filtering by time, cutting off the end" in {
    val nodes = datasetTree.getData(0, 101, 50)
    nodes.length should be(3)

    val node0 = nodes(0).asInstanceOf[BTreeNode[FloatAggregator]]
    node0.minTime should be(0)
    node0.maxTime should be(1)
    node0.nodeData.count should be(2)
    node0.nodeData.sum should be(0 + 1)

    val node1 = nodes(1).asInstanceOf[BTreeLeaf[FloatAggregator]]
    node1.point should be((2, 2))
    val node2 = nodes(2).asInstanceOf[BTreeLeaf[FloatAggregator]]
    node2.point should be((100, 3))
  }

  it should "return getNodes filtering by time, cutting off both start and end" in {
    val nodes = datasetTree.getData(3, 101, 50)
    nodes.length should be(1)

    val node1 = nodes(0).asInstanceOf[BTreeLeaf[FloatAggregator]]
    node1.point should be((100, 3))
  }
}
