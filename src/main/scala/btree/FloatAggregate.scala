package bigvis
package btree

case class FloatAggregate(
                             min: Float,
                             max: Float,
                             count: Long,
                             sum: Float,
                         ) {
  def mean: Float = sum / count
}

object FloatAggregator extends BTreeAggregator {
  override type NodeType = FloatAggregate
  override type LeafType = Float

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
