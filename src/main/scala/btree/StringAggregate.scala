package bigvis
package btree

case class StringAggregate(
                           count: Long,
                           summary: Option[String],
                         )

object StringAggregator extends BTreeAggregator {
  override type NodeType = StringAggregate
  override type LeafType = String

  override def fromLeaves(data: Seq[(BTree.TimestampType, String)]): StringAggregate = {
    val allData = data.map(_._2)
    allData.distinct match {
      case Seq(value) => StringAggregate(allData.size, Some(value))
      case _ => StringAggregate(allData.size, None)
    }
  }

  override def fromNodes(data: Seq[((BTree.TimestampType, BTree.TimestampType), StringAggregate)]): StringAggregate = {
    val allNodes = data.map(_._2)
    allNodes.map(_.summary).distinct match {
      case Seq(value) => StringAggregate(allNodes.map(_.count).sum, value)
      case _ => StringAggregate(allNodes.map(_.count).sum, None)
    }
  }
}
