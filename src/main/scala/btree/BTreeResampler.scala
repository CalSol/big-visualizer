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
    val chunks = ChunkSeq[BTreeData[AggregatorType], Option[BTree.TimestampType]](data, None, {
      case (None, elem: BTreeLeaf[AggregatorType]) =>
        println(s"new / leaf ${elem.point._1} $elem: new seq")
        (Some(elem.point._1), false)
      case (None, elem: BTreeAggregate[AggregatorType]) =>
        println(s"new / node $elem: new seq")
        (Some(elem.minTime), false)
      case (Some(prevBegin), elem: BTreeLeaf[AggregatorType]) =>
        if ((elem.point._1 - prevBegin >= minResolution) ||
            (elem.point._1 % minResolution < prevBegin % minResolution)) {
          println(s"prev $prevBegin / leaf ${elem.point._1} $elem: new seq")
          (Some(elem.point._1), true)
        } else {
          println(s"prev $prevBegin / leaf ${elem.point._1} $elem: cont seq")
          (Some(prevBegin), false)
        }
      case (Some(prevBegin), elem: BTreeAggregate[AggregatorType]) =>
        if ((elem.maxTime - prevBegin >= minResolution) ||
            (elem.maxTime % minResolution < prevBegin % minResolution)) {
          println(s"prev $prevBegin / node $elem: new seq")
          (Some(elem.minTime), true)
        } else {
          println(s"prev $prevBegin / node $elem: cont seq")
          (Some(prevBegin), false)
        }
      })

    val aggregatedChunks = chunks.map { chunk =>
      val nodeChunks = chunk.map {
        case elem: BTreeLeaf[AggregatorType] =>
          val node = aggregator.fromLeaves(Seq(elem.point.asInstanceOf[(BTree.TimestampType, aggregator.LeafType)]))
          ((elem.point._1, elem.point._1), node)
        case elem: BTreeAggregate[AggregatorType] =>
          ((elem.minTime, elem.maxTime), elem.nodeData.asInstanceOf[aggregator.NodeType])
      }
      val combinedNode = aggregator.fromNodes(nodeChunks)
      new BTreeResampledNode[AggregatorType](nodeChunks.head._1._1, nodeChunks.last._1._2, combinedNode)
    }
    aggregatedChunks
  }
}
