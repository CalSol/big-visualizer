package bigvis
package btree

case class FloatArrayAggregate(
                             min: Float,
                             max: Float,
                             count: Long,
                             sum: Float,
                         ) {
  def mean: Float = sum / count
}

class FloatArrayAggregator extends BTreeAggregator {
  override type NodeType = FloatArrayAggregate
  override type LeafType = Seq[Float]

  override def fromLeaves(data: Seq[(BTree.TimestampType, Seq[Float])]): FloatArrayAggregate = {
    val allData = data.flatMap(_._2)
    FloatArrayAggregate(
      allData.min, allData.max,
      allData.length, allData.sum)
  }

  override def fromNodes(data: Seq[((BTree.TimestampType, BTree.TimestampType), FloatArrayAggregate)]): FloatArrayAggregate = {
    val allNodes = data.map(_._2)
    FloatArrayAggregate(
      allNodes.map(_.min).min, allNodes.map(_.max).max,
      allNodes.map(_.count).sum, allNodes.map(_.sum).sum
    )
  }
}

object FloatArrayAggregator {
  val aggregator = new FloatArrayAggregator()
}
