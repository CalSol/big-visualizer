package bigvis
package btree


/** Data from a B-tree which has been flattened into a linear continuous series (even if at different levels
  * of detail) and sectioned according to some maximum time break parameter.
  */
class SectionedData[AggregatorType <: BTreeAggregator](sections: IndexedSeq[IndexedSeq[BTreeData[AggregatorType]]]) {
  private def dataToStartTime(data: BTreeData[AggregatorType]): Long = data match {
    case leaf: BTreeLeaf[AggregatorType] => leaf.point._1
    case aggr: BTreeAggregate[AggregatorType] => aggr.minTime
  }

  private def dataToEndTime(data: BTreeData[AggregatorType]): Long = data match {
    case leaf: BTreeLeaf[AggregatorType] => leaf.point._1
    case aggr: BTreeAggregate[AggregatorType] => aggr.maxTime
  }

  def getClosestValue(time: BTree.TimestampType, tolerance: BTree.TimestampType): Option[BTreeData[AggregatorType]] = {
    // Find the section nearest the requested time point
    val sectionsIntervals = sections.map { section =>
      (dataToStartTime(section.head), dataToEndTime(section.last))
    }
    SearchInterval(sectionsIntervals, time, tolerance).map { result =>
      sections(result.index())
    }.flatMap { section => // Then find the data point within the section
      val sectionIntervals = section.map { node =>
        (dataToStartTime(node), dataToEndTime(node))
      }
      SearchInterval(sectionIntervals, time, tolerance).map { result =>
        section(result.index())
      }
    }
  }
}
