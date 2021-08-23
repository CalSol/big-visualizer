package bigvis

import scala.collection.mutable

object BTree {
  type TimestampType = Long
}

abstract class BTreeAggregator[NodeType, LeafType] {
  // Compute intermediate node data, for a node consisting of only leaves
  // Must be able to handle the empty case
  def fromLeaves(data: Seq[(BTree.TimestampType, LeafType)]): NodeType
  // Compute intermediate node data, for a node consisting of only sub-nodes
  // Must be able to handle the empty case
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
  // Adds the data (as points of (timestamp, data), Data must be ordered, but only within itself
  // (it can overlap with existing points in the tree)
  // Data must not be empty.
  def appendAll(data: IterableOnce[(BTree.TimestampType, LeafType)])

  // Returns all the leaf points as a seq
  def toSeq: Seq[(BTree.TimestampType, LeafType)]

  // Returns the maximum depth of the tree, excluding leaf entries
  // Expensive! This traverses the entire tree!
  def maxDepth: Int
}

// Internal data structure, base class for a tree node
abstract class BTreeNode[NodeType, LeafType](root: BTree[NodeType, LeafType]) {
  def minTime: BTree.TimestampType  // returns the lowest timestamp in this node
  def maxTime: BTree.TimestampType  // returns the highest timestamp in this node
  def data: NodeType  // return the aggregate data

  // The initial / down (from root) pass to insert data
  // The data passed into each node must be the data to be inserted into it, it is the caller's
  // responsibility that the right data gets to the right sub-node
  // Returns any data that couldn't be added, because the node is full, as a signal to the caller to split
  // the called node
  def appendAll(data: Seq[(BTree.TimestampType, LeafType)]): Seq[(BTree.TimestampType, LeafType)]

  def validate(): Boolean  // consistency check - very expensive operation!
}

// B-tree node that contains an array of leaves
class BTreeLeafNode[NodeType, LeafType](root: BTree[NodeType, LeafType],
                                        parent: Option[BTreeNode[NodeType, LeafType]])
    extends BTreeNode[NodeType, LeafType](root) {
  protected val leaves = mutable.ArrayBuffer[(BTree.TimestampType, LeafType)]()
  protected var data: NodeType = root.aggregator.fromLeaves(Seq())  // intermediate node data

  // Initialized with invalid values when empty
  protected var minTime: BTree.TimestampType = Long.MaxValue
  protected var maxTime: BTree.TimestampType = Long.MinValue

  def appendAll(data: Seq[(BTree.TimestampType, LeafType)]): Seq[(BTree.TimestampType, LeafType)] = {
    // Insert data until full
    // When full, split the node in the parent, recursively as needed, and delegate continued data adding
    require(data.nonEmpty)  // empty appends handled at BTree level
  }

  def validate(): Boolean = {
    // TODO: validation fails on empty leaves
    minTime == leaves.head._1 && maxTime == leaves.last._1 && leaves.size <= root.nodeSize &&
        data == root.aggregator.fromLeaves(leaves.toSeq)
  }
}

// B-tree node that contains an array of other nodes
class BTreeIntermediateNode[NodeType, LeafType](root: BTree[NodeType, LeafType],
                                                parent: Option[BTreeNode[NodeType, LeafType]])
    extends BTreeNode[NodeType, LeafType](root) {
  protected val nodes = mutable.ArrayBuffer[BTreeNode[NodeType, LeafType]]()
  protected var data: NodeType = root.aggregator.fromNodes(Seq())  // intermediate node data

  // Initialized with invalid values when empty
  protected var minTime: BTree.TimestampType = Long.MaxValue
  protected var maxTime: BTree.TimestampType = Long.MinValue

  def appendAll(data: Seq[(BTree.TimestampType, LeafType)]): Seq[(BTree.TimestampType, LeafType)] = {
    // Insert data until full
    // When full, split the node in the parent, recursively as needed
    require(data.nonEmpty)  // empty appends handled at BTree level
  }

  def validate(): Boolean = {
    val timesOrdered = nodes.toSeq.grouped(2).map { case Seq(prev, next) =>
      prev.maxTime <= next.minTime
    }.forall(_ == true)
    val expectedData = root.aggregator.fromNodes(nodes.map { node =>
      ((node.minTime, node.maxTime), node.data)
    }.toSeq)
    nodes.nonEmpty && minTime == nodes.head.minTime && maxTime == nodes.last.maxTime && timesOrdered &&
        nodes.size <= root.nodeSize && data == expectedData
  }
}

