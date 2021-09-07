package bigvis
package btree

import scala.collection.mutable

object BTree {
  type TimestampType = Long
}

abstract class BTreeAggregator {
  type NodeType
  type LeafType
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
class BTree[AggregatorType <: BTreeAggregator](aggregator: AggregatorType,
                                               val nodeSize: Int) {
  // TODO debug why the type checker chokes without explicit casts
  def aggregateFromLeaves(data: Seq[(BTree.TimestampType, AggregatorType#LeafType)]): AggregatorType#NodeType =
    aggregator.fromLeaves(data.asInstanceOf[Seq[(BTree.TimestampType, this.aggregator.LeafType)]])

  def aggregateFromNodes(data: Seq[((BTree.TimestampType, BTree.TimestampType), AggregatorType#NodeType)]): AggregatorType#NodeType =
    aggregator.fromNodes(data.asInstanceOf[Seq[((BTree.TimestampType, BTree.TimestampType), this.aggregator.NodeType)]])

  protected var root: BTreeNode[AggregatorType] = new BTreeLeafNode(this)

  // Adds the data (as points of (timestamp, data), Data must be ordered, but only within itself
  // (it can overlap with existing points in the tree)
  // Data must not be empty.
  def appendAll(data: IterableOnce[(BTree.TimestampType, AggregatorType#LeafType)]): Unit = {
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
  def toSeq: Seq[(BTree.TimestampType, AggregatorType#LeafType)] = {
    toLeafChain.flatMap(_.leaves)
  }

  protected def toLeafChain: Seq[BTreeLeafNode[AggregatorType]] = {
    def traverse(node: BTreeNode[AggregatorType]): Seq[BTreeLeafNode[AggregatorType]] = node match {
      case node: BTreeIntermediateNode[AggregatorType] => node.nodes.toSeq flatMap(traverse)
      case node: BTreeLeafNode[AggregatorType] => Seq(node)
    }
    traverse(root)
  }

  // Returns the maximum depth of the tree, excluding leaf entries
  // Expensive! This traverses the entire tree!
  def maxDepth: Int = {
    def traverse(node: BTreeNode[AggregatorType]): Int = node match {
      case node: BTreeIntermediateNode[AggregatorType] => node.nodes.toSeq.map(traverse).max + 1
      case node: BTreeLeafNode[AggregatorType] => 1
    }
    traverse(root)
  }

  // Returns a list of nodes (or leaves), from startTime (inclusive) to endTime (exclusive),
  // where any intermediate nodes span at most minResolution in time
  def getData(startTime: BTree.TimestampType, endTime: BTree.TimestampType,
               minResolution: BTree.TimestampType): Seq[BTreeData[AggregatorType]] = {
    def traverse(node: BTreeNode[AggregatorType]): Seq[BTreeData[AggregatorType]] = {
      if (node.maxTime < startTime || node.minTime >= endTime) {  // out of time bounds
        Seq()
      } else if ((node.maxTime - node.minTime) <= minResolution) {  // minimum resolution
        Seq(node)
      } else {
        node match {
          case node: BTreeIntermediateNode[AggregatorType] => node.nodes.toSeq.flatMap(traverse)
          case node: BTreeLeafNode[AggregatorType] => node.leaves.toSeq.map {
            new BTreeLeaf[AggregatorType](_)
          }
        }
      }
    }
    traverse(root)
  }

  def rootData: AggregatorType#NodeType = root.nodeData

  def minTime: BTree.TimestampType = root.minTime
  def maxTime: BTree.TimestampType = root.maxTime

  def validate(): Boolean = root.validate()
}

sealed abstract class BTreeData[AggregatorType <: BTreeAggregator]

class BTreeLeaf[AggregatorType <: BTreeAggregator](val point: (BTree.TimestampType, AggregatorType#LeafType))
    extends BTreeData[AggregatorType] {
}

// Internal data structure, base class for a tree node
sealed abstract class BTreeNode[AggregatorType <: BTreeAggregator](root: BTree[AggregatorType])
    extends BTreeData[AggregatorType] {
  def minTime: BTree.TimestampType  // returns the lowest timestamp in this node
  def maxTime: BTree.TimestampType  // returns the highest timestamp in this node
  def nodeData: AggregatorType#NodeType  // return the aggregate data

  // The initial / down (from root) pass to insert data
  // The data passed into each node must be the data to be inserted into it, it is the caller's
  // responsibility that the right data gets to the right sub-node
  // Returns any data that couldn't be added, because the node is full, as a signal to the caller to split
  // the called node
  def appendAll(data: Seq[(BTree.TimestampType, AggregatorType#LeafType)]): Seq[(BTree.TimestampType, AggregatorType#LeafType)]

  // Splits this node, updating this node and returning the split off node.
  // This node contains the lower half of timestamps, and the split off node contains the upper half.
  def split(): BTreeNode[AggregatorType]

  def validate(): Boolean  // consistency check - very expensive operation!
}

// B-tree node that contains an array of leaves
class BTreeLeafNode[AggregatorType <: BTreeAggregator](root: BTree[AggregatorType])
    extends BTreeNode[AggregatorType](root) {
  protected[bigvis] var leaves = mutable.ArrayBuffer[(BTree.TimestampType, AggregatorType#LeafType)]()
  protected var internalNodeData: Option[AggregatorType#NodeType] = None  // intermediate node data
  override def nodeData: AggregatorType#NodeType = internalNodeData.get

  // Initialized with invalid values when empty
  protected var internalMinTime = Long.MaxValue
  override def minTime: BTree.TimestampType = {
    require(leaves.nonEmpty)
    internalMinTime
  }
  protected var internalMaxTime = Long.MinValue
  override def maxTime: BTree.TimestampType = {
    require(leaves.nonEmpty)
    internalMaxTime
  }

  def appendAll(data: Seq[(BTree.TimestampType, AggregatorType#LeafType)]): Seq[(BTree.TimestampType, AggregatorType#LeafType)] = {
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
    internalNodeData = Some(root.aggregateFromLeaves(leaves.toSeq))

    remainingData  // return any remaining data - indicates this node needs to be split
  }

  def split(): BTreeLeafNode[AggregatorType] = {
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
    internalNodeData = Some(root.aggregateFromLeaves(leaves.toSeq))

    rightNode
  }

  def validate(): Boolean = {
    require(leaves.nonEmpty)  // TODO: validation fails on empty leaves
    internalMinTime == leaves.head._1 && internalMaxTime == leaves.last._1 && leaves.size <= root.nodeSize &&
        internalNodeData.get == root.aggregateFromLeaves(leaves.toSeq)
  }
}

// B-tree node that contains an array of other nodes
class BTreeIntermediateNode[AggregatorType <: BTreeAggregator](root: BTree[AggregatorType])
    extends BTreeNode[AggregatorType](root) {
  protected[bigvis] var nodes = mutable.ArrayBuffer[BTreeNode[AggregatorType]]()
  protected var internalNodeData: Option[AggregatorType#NodeType] = None  // intermediate node data
  override def nodeData: AggregatorType#NodeType = internalNodeData.get

  // Initialized with invalid values when empty
  protected var internalMinTime = Long.MaxValue
  override def minTime: BTree.TimestampType = {
    require(nodes.nonEmpty)
    internalMinTime
  }
  protected var intermalMaxTime = Long.MinValue
  override def maxTime: BTree.TimestampType = {
    require(nodes.nonEmpty)
    intermalMaxTime
  }

  def appendAll(data: Seq[(BTree.TimestampType, AggregatorType#LeafType)]): Seq[(BTree.TimestampType, AggregatorType#LeafType)] = {
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
    internalNodeData = Some(root.aggregateFromNodes(nodes.toSeq.map { node =>
      ((node.minTime, node.maxTime), node.nodeData)
    }))

    remainingData.toSeq
  }


  // Internal API to directly add nodes, instead of appending data points
  protected[bigvis] def appendNodes(addNodes: Seq[BTreeNode[AggregatorType]]): Unit = {
    require(nodes.isEmpty, "can't appendNodes with existing nodes")
    require(addNodes.length <= root.nodeSize, "appendNodes must not overflow")
    nodes.appendAll(addNodes)

    internalMinTime = nodes.head.minTime
    intermalMaxTime = nodes.last.maxTime
    internalNodeData = Some(root.aggregateFromNodes(nodes.toSeq.map { node =>
      ((node.minTime, node.maxTime), node.nodeData)
    }))

  }

  def split(): BTreeIntermediateNode[AggregatorType] = {
    require(nodes.length == root.nodeSize, "can't split before at max size")
    val (leftSplit, rightSplit) = nodes.splitAt(root.nodeSize / 2)

    // create right node
    val rightNode = new BTreeIntermediateNode(root)
    rightNode.appendNodes(rightSplit.toSeq)

    // update this node
    nodes = leftSplit
    internalMinTime = nodes.head.minTime
    intermalMaxTime = nodes.last.maxTime
    internalNodeData = Some(root.aggregateFromNodes(nodes.toSeq.map { node =>
      ((node.minTime, node.maxTime), node.nodeData)
    }))

    rightNode
  }

  def validate(): Boolean = {
    val nodesValidated = nodes.map { node => node.validate() }.forall(_ == true)
    val timesOrdered = nodes.toSeq.sliding(2).map { case Seq(prev, next) =>
      prev.maxTime <= next.minTime
    }.forall(_ == true)
    val expectedData = root.aggregateFromNodes(nodes.map { node =>
      ((node.minTime, node.maxTime), node.nodeData)
    }.toSeq)
    nodes.nonEmpty && internalMinTime == nodes.head.minTime && intermalMaxTime == nodes.last.maxTime &&
        nodesValidated && timesOrdered &&
        nodes.size <= root.nodeSize && internalNodeData.get == expectedData
  }
}
