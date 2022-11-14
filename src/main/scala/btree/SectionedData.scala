package bigvis
package btree


object SectionedData {
  private def dataToStartTime[AggregatorType <: BTreeAggregator](data: BTreeData[AggregatorType]): Long = data match {
    case leaf: BTreeLeaf[AggregatorType] => leaf.point._1
    case aggr: BTreeAggregate[AggregatorType] => aggr.minTime
  }

  private def dataToEndTime[AggregatorType <: BTreeAggregator](data: BTreeData[AggregatorType]): Long = data match {
    case leaf: BTreeLeaf[AggregatorType] => leaf.point._1
    case aggr: BTreeAggregate[AggregatorType] => aggr.maxTime
  }

  // creates SectionedData from BTree getData, chunking by breakTime breaks
  def from[AggregatorType <: BTreeAggregator](nodes: Seq[BTreeData[AggregatorType]], initVal: BTree.TimestampType,
                                              breakTime: BTree.TimestampType): SectionedData[AggregatorType] = {
    val sectioned = ChunkSeq(nodes, initVal, (prevTime: Long, elem: BTreeData[AggregatorType]) => {
      elem match {
        case node: BTreeAggregate[AggregatorType] =>
          (node.maxTime, node.minTime > prevTime + breakTime)
        case node: BTreeLeaf[AggregatorType] => // TODO return individual data points
          (node.point._1, node.point._1 > prevTime + breakTime)
      }
    }).map{_.toIndexedSeq}
    new SectionedData(sectioned.toIndexedSeq)
  }
}


/** Data from a B-tree which has been flattened into a linear continuous series (even if at different levels
  * of detail) and sectioned according to some maximum time break parameter.
  */
class SectionedData[AggregatorType <: BTreeAggregator](sections: IndexedSeq[IndexedSeq[BTreeData[AggregatorType]]]) {
  def data: IndexedSeq[IndexedSeq[BTreeData[AggregatorType]]] = sections

  def getClosestValue(time: BTree.TimestampType, tolerance: BTree.TimestampType): Option[BTreeData[AggregatorType]] = {
    // Find the section nearest the requested time point
    val sectionsIntervals = sections.map { section =>
      (SectionedData.dataToStartTime(section.head), SectionedData.dataToEndTime(section.last))
    }
    SearchInterval(sectionsIntervals, time, tolerance).map { result =>
      sections(result.index())
    }.flatMap { section => // Then find the data point within the section
      val sectionIntervals = section.map { node =>
        (SectionedData.dataToStartTime(node), SectionedData.dataToEndTime(node))
      }
      SearchInterval(sectionIntervals, time, tolerance).map { result =>
        section(result.index())
      }
    }
  }
}
