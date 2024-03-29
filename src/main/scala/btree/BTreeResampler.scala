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
    val chunks = ChunkSeq[BTreeData[AggregatorType], BTree.TimestampType](data, Long.MinValue, (startTime, node) =>
      if ((node.maxTime >= startTime + minResolution)
        || (node.maxTime % minResolution < startTime % minResolution)) {  // force realignment at minResolution bounds
        (node.minTime, true)
      } else {
        (startTime, false)
      }
    )

    val aggregatedChunks = chunks.map { chunk =>
      if (chunk.length > 1) {
        val nodeChunks = chunk.map {
          case elem: BTreeLeaf[AggregatorType] =>
            val node = aggregator.fromLeaves(Seq(elem.point.asInstanceOf[(BTree.TimestampType, aggregator.LeafType)]))
            ((elem.point._1, elem.point._1), node)
          case elem: BTreeAggregate[AggregatorType] =>
            ((elem.minTime, elem.maxTime), elem.nodeData.asInstanceOf[aggregator.NodeType])
        }
        val combinedNode = aggregator.fromNodes(nodeChunks)
        new BTreeResampledNode[AggregatorType](nodeChunks.head._1._1, nodeChunks.last._1._2, combinedNode)
      } else if (chunk.length == 1) {
        chunk.head
      } else {
        throw new RuntimeException("unexpected empty chunk")
      }
    }
    aggregatedChunks
  }
}
