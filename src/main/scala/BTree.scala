package bigvis

import scala.collection.mutable

object BTree {
  type TimestampType = Long
}

abstract class BTreeAggregator[NodeType, LeafType] {
  // Compute intermediate node data, for a node consisting of only leaves
  def fromLeaves(data: Seq[(BTree.TimestampType, LeafType)]): NodeType
  // Compute intermediate node data, for a node consisting of only sub-nodes
  def fromNodes(data: Seq[((BTree.TimestampType, BTree.TimestampType), NodeType)]): NodeType
}

/** A mutable B-Tree for timeseries, where data points have some kind of type-paramterized data
 * and an integer timestamp.
 * See https://en.wikipedia.org/wiki/B-tree
 *
 * The nodeSize parameter is the maximum number of children it has ("order" / m in the Wikipedia page).
 */
abstract class BTree[NodeType, LeafType](val aggregator: BTreeAggregator[NodeType, LeafType],
                                         val nodeSize: Int) {
  // Adds the data (as points of (timestamp, data)
  def appendAll(data: IterableOnce[(BTree.TimestampType, LeafType)])

  // Returns all the leaf points as a seq
  def toSeq: Seq[(BTree.TimestampType, LeafType)]
}

// Internal data structure, base class for a tree node
abstract class BTreeNode[NodeType, LeafType](parent: BTree[NodeType, LeafType]) {
  def minTime: BTree.TimestampType  // returns the lowest timestamp in this node
  def maxTime: BTree.TimestampType  // returns the highest timestamp in this node

  def appendAll(data: IterableOnce[(BTree.TimestampType, LeafType)])
  def validate(): Boolean  // consistency check
}

// B-tree node that contains an array of leaves
abstract class BTreeLeafNode[NodeType, LeafType](parent: BTree[NodeType, LeafType])
    extends BTreeNode[NodeType, LeafType] {
  protected val leaves = mutable.ArrayBuffer[(BTree.TimestampType, LeafType)]()
  protected var data: NodeType = parent.aggregator.fromLeaves(Seq())  // intermediate node data
}

// B-tree node that contains an array of other nodes
abstract class BTreeIntermediateNode[NodeType, LeafType](parent: BTree[NodeType, LeafType])
    extends BTreeNode[NodeType, LeafType] {
  protected val nodes = mutable.ArrayBuffer[(BTree.TimestampType, BTreeNode[NodeType, LeafType])]()
  protected var data: NodeType = parent.aggregator.fromNodes(Seq())  // intermediate node data
  
}

