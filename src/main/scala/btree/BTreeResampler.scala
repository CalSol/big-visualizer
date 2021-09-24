package bigvis
package btree

import collection.mutable

object BTreeResampler {
  /** Given the raw return from BTree.getData (sequence of BTreeData), returns a new sequence
   * with samples aggregated as much as possible while still staying within minResolution.
   *
   * For example, if the BTree had nodes that were 10ms apart, with the next level up 80ms apart,
   * but we asked to a minResolution of 40ms, the BTree would return the 10ms resolution nodes.
   * This post-processes those nodes by aggregating them to 40ms buckets.
   *
   * This forces an alignment boundary at integer multiples of minResolution. A new aggregate
   * will be started when crossing these boundary, even if the prior aggregate was not full.
   * Otherwise, a greedy naive algorithm can return a different set of aggregates every time.
   */
  def apply[AggregatorType <: BTreeAggregator](
      aggregator: AggregatorType,
      data: Seq[BTreeData[AggregatorType]], minResolution: BTree.TimestampType): Seq[BTreeData[AggregatorType]] = {
    // Chunk by time
    val groups = ChunkSeq[BTreeData[AggregatorType], Option[BTree.TimestampType]](data, None, {
      case (None, elem: BTreeLeaf[AggregatorType]) => (Some(elem.point._1), false)
      case (None, elem: BTreeAggregate[AggregatorType]) => (Some(elem.minTime), false)
      case (Some(prevBegin), elem: BTreeLeaf[AggregatorType]) =>
        if ((elem.point._1 - prevBegin > minResolution) ||
            (elem.point._1 % minResolution < prevBegin % minResolution)) {
          (None, true)
        } else {
          (Some(prevBegin), false)
        }
      case (Some(prevBegin), elem: BTreeAggregate[AggregatorType]) =>
        if ((elem.maxTime - prevBegin > minResolution) ||
            (elem.maxTime % minResolution < prevBegin % minResolution)) {
          (None, true)
        } else {
          (Some(prevBegin), false)
        }
      })

    // Where a chunk in time has multiple entries, aggregate it
    val aggregated = mutable.ArrayBuffer[BTreeNode[AggregatorType]]()
    val leavesBuffer = mutable.ArrayBuffer[AggregatorType#LeafType]()
    ???
  }
}
