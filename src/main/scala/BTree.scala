package bigvis

import scala.collection.mutable

object BTree {
  type TimestampType = Long
}

abstract class BTreeAggregator[NodeType, LeafType] {
  // Compute intermediate node data, for a node consisting of only leaves.
  // data guaranteed non-empty
  def fromLeaves(data: Seq[(BTree.TimestampType, LeafType)]): NodeType
  // Compute intermediate node data, for a node consisting of only sub-nodes
  // data guaranteed non-empty
  def fromNodes(data: Seq[((BTree.TimestampType, BTree.TimestampType), NodeType)]): NodeType
}

/** A mutable B-Tree for timeseries, where data points have some kind of type-paramterized data
 * and an integer timestamp.
 * See https://en.wikipedia.org/wiki/B-tree
 *
 * The nodeSize parameter is the maximum number of children it has ("order" / m in the Wikipedia page).
 */
class BTree[NodeType, LeafType](val aggregator: BTreeAggregator[NodeType, LeafType],
                                val nodeSize: Int) {
  protected var root: BTreeNode[NodeType, LeafType] = new BTreeLeafNode(this)

  // Adds the data (as points of (timestamp, data), Data must be ordered, but only within itself
  // (it can overlap with existing points in the tree)
  // Data must not be empty.
  def appendAll(data: IterableOnce[(BTree.TimestampType, LeafType)]): Unit = {
    var remainingData = data.toSeq
    while (remainingData.nonEmpty) {
      remainingData = root.appendAll(remainingData)
      if (remainingData.nonEmpty) {  // split node and insert new root
        val leftNode = root
        val rightNode = root.split()

        val newRoot = new BTreeIntermediateNode(this)
        newRoot.appendNodes(Seq(leftNode, rightNode))
        root = newRoot
      }
    }
  }

  // Returns all the leaf points as a seq
  def toSeq: Seq[(BTree.TimestampType, LeafType)] = {
    toLeafChain.flatMap(_.leaves)
  }

  protected def toLeafChain: Seq[BTreeLeafNode[NodeType, LeafType]] = {
    def traverse(node: BTreeNode[NodeType, LeafType]): Seq[BTreeLeafNode[NodeType, LeafType]] = node match {
      case node: BTreeIntermediateNode[NodeType, LeafType] => node.nodes.toSeq flatMap(traverse)
      case node: BTreeLeafNode[NodeType, LeafType] => Seq(node)
    }
    traverse(root)
  }

  // Returns the maximum depth of the tree, excluding leaf entries
  // Expensive! This traverses the entire tree!
  def maxDepth: Int = {
    def traverse(node: BTreeNode[NodeType, LeafType]): Int = node match {
      case node: BTreeIntermediateNode[NodeType, LeafType] => node.nodes.toSeq.map(traverse).max + 1
      case node: BTreeLeafNode[NodeType, LeafType] => 1
    }
    traverse(root)
  }
}

// Internal data structure, base class for a tree node
abstract class BTreeNode[NodeType, LeafType](root: BTree[NodeType, LeafType]) {
  def minTime: BTree.TimestampType  // returns the lowest timestamp in this node
  def maxTime: BTree.TimestampType  // returns the highest timestamp in this node
  def nodeData: NodeType  // return the aggregate data

  // The initial / down (from root) pass to insert data
  // The data passed into each node must be the data to be inserted into it, it is the caller's
  // responsibility that the right data gets to the right sub-node
  // Returns any data that couldn't be added, because the node is full, as a signal to the caller to split
  // the called node
  def appendAll(data: Seq[(BTree.TimestampType, LeafType)]): Seq[(BTree.TimestampType, LeafType)]

  // Splits this node, updating this node and returning the split off node.
  // This node contains the lower half of timestamps, and the split off node contains the upper half.
  def split(): BTreeNode[NodeType, LeafType]

  def validate(): Boolean  // consistency check - very expensive operation!
}

// B-tree node that contains an array of leaves
class BTreeLeafNode[NodeType, LeafType](root: BTree[NodeType, LeafType])
    extends BTreeNode[NodeType, LeafType](root) {
  protected[bigvis] var leaves = mutable.ArrayBuffer[(BTree.TimestampType, LeafType)]()
  protected var internalNodeData: Option[NodeType] = None  // intermediate node data
  override def nodeData: NodeType = internalNodeData.get

  // Initialized with invalid values when empty
  protected var internalMinTime: BTree.TimestampType = Long.MaxValue
  override def minTime = internalMinTime
  protected var internalMaxTime: BTree.TimestampType = Long.MinValue
  override def maxTime = internalMaxTime

  def appendAll(data: Seq[(BTree.TimestampType, LeafType)]): Seq[(BTree.TimestampType, LeafType)] = {
    // Insert data until full
    // When full, split the node in the parent, recursively as needed, and delegate continued data adding
    require(data.nonEmpty)  // empty appends handled at BTree level
    require(data.head._1 >= internalMaxTime, "TODO: support insertions not at end")  // I'm a lazy duck

    var currTime = internalMaxTime
    var remainingData = data
    while (leaves.length < root.nodeSize && remainingData.nonEmpty) {
      val head = remainingData.head
      require(head._1 >= currTime)
      currTime = head._1
      leaves.append(head)
      remainingData = remainingData.tail
    }

    // update this node
    internalMinTime = leaves.head._1
    internalMaxTime = leaves.last._1
    internalNodeData = Some(root.aggregator.fromLeaves(leaves.toSeq))

    remainingData  // return any remaining data - indicates this node needs to be split
  }

  def split(): BTreeLeafNode[NodeType, LeafType] = {
    require(leaves.length == root.nodeSize, "can't split before at max size")
    val (leftSplit, rightSplit) = leaves.splitAt(root.nodeSize / 2)

    // create right node
    val rightNode = new BTreeLeafNode(root)
    val rightLeftover = rightNode.appendAll(rightSplit.toSeq)
    require(rightLeftover.isEmpty, "split should not overflow")

    // update this node
    leaves = leftSplit
    internalMinTime = leaves.head._1
    internalMaxTime = leaves.last._1
    internalNodeData = Some(root.aggregator.fromLeaves(leaves.toSeq))

    rightNode
  }

  def validate(): Boolean = {
    // TODO: validation fails on empty leaves
    internalMinTime == leaves.head._1 && internalMaxTime == leaves.last._1 && leaves.size <= root.nodeSize &&
        internalNodeData == root.aggregator.fromLeaves(leaves.toSeq)
  }
}

// B-tree node that contains an array of other nodes
class BTreeIntermediateNode[NodeType, LeafType](root: BTree[NodeType, LeafType])
    extends BTreeNode[NodeType, LeafType](root) {
  protected[bigvis] var nodes = mutable.ArrayBuffer[BTreeNode[NodeType, LeafType]]()
  protected var internalNodeData: Option[NodeType] = None  // intermediate node data
  override def nodeData: NodeType = internalNodeData.get

  // Initialized with invalid values when empty
  protected var internalMinTime: BTree.TimestampType = Long.MaxValue
  override def minTime = internalMinTime
  protected var intermalMaxTime: BTree.TimestampType = Long.MinValue
  override def maxTime = intermalMaxTime

  def appendAll(data: Seq[(BTree.TimestampType, LeafType)]): Seq[(BTree.TimestampType, LeafType)] = {
    // Insert data until full
    // When full, split the node in the parent, recursively as needed
    if (nodes.isEmpty) {
      nodes.append(new BTreeLeafNode(root))
    }
    require(data.head._1 >= intermalMaxTime, "TODO: support insertions not at end")  // I'm a lazy duck

    var remainingData = nodes.last.appendAll(data)
    while (remainingData.nonEmpty && nodes.length < root.nodeSize) {  // while this node can still do useful work
      // Split the node we just inserted into
      val newLast = nodes.last.split()
      nodes.append(newLast)

      remainingData = newLast.appendAll(remainingData)
    }

    internalMinTime = nodes.head.minTime
    intermalMaxTime = nodes.last.maxTime
    internalNodeData = Some(root.aggregator.fromNodes(nodes.toSeq.map { node =>
      ((node.minTime, node.maxTime), node.nodeData)
    }))

    remainingData.toSeq
  }


  // Internal API to directly add nodes, instead of appending data points
  protected[bigvis] def appendNodes(addNodes: Seq[BTreeNode[NodeType, LeafType]]): Unit = {
    require(nodes.isEmpty, "can't appendNodes with existing nodes")
    require(addNodes.length <= root.nodeSize, "appendNodes must not overflow")
    nodes.appendAll(addNodes)

    internalMinTime = nodes.head.minTime
    intermalMaxTime = nodes.last.maxTime
    internalNodeData = Some(root.aggregator.fromNodes(nodes.toSeq.map { node =>
      ((node.minTime, node.maxTime), node.nodeData)
    }))

  }

  def split(): BTreeIntermediateNode[NodeType, LeafType] = {
    require(nodes.length == root.nodeSize, "can't split before at max size")
    val (leftSplit, rightSplit) = nodes.splitAt(root.nodeSize / 2)

    // create right node
    val rightNode = new BTreeIntermediateNode(root)
    rightNode.appendNodes(rightSplit.toSeq)

    // update this node
    nodes = leftSplit
    internalMinTime = nodes.head.minTime
    intermalMaxTime = nodes.last.maxTime
    internalNodeData = Some(root.aggregator.fromNodes(nodes.toSeq.map { node =>
      ((node.minTime, node.maxTime), node.nodeData)
    }))

    rightNode
  }

  def validate(): Boolean = {
    val nodesValidated = nodes.map { node => node.validate() }.forall(_ == true)
    val timesOrdered = nodes.toSeq.grouped(2).map { case Seq(prev, next) =>
      prev.maxTime <= next.minTime
    }.forall(_ == true)
    val expectedData = root.aggregator.fromNodes(nodes.map { node =>
      ((node.minTime, node.maxTime), node.nodeData)
    }.toSeq)
    nodes.nonEmpty && internalMinTime == nodes.head.minTime && intermalMaxTime == nodes.last.maxTime &&
        nodesValidated && timesOrdered &&
        nodes.size <= root.nodeSize && internalNodeData == expectedData
  }
}
