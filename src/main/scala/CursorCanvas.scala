package bigvis

import btree.{BTreeAggregate, BTreeData, BTreeLeaf, FloatAggregator}


class CursorCanvas extends ResizableCanvas {
  def draw(scale: ChartParameters, cursorPos: Double,
           datasetValues: Seq[(ChartDefinition, Option[BTreeData[FloatAggregator]])]): Unit = {
    val gc = getGraphicsContext2D

    gc.clearRect(0, 0, scale.width, scale.height)

    val cursorTime = scale.xPosToVal(cursorPos)
    gc.strokeLine(cursorPos, 0, cursorPos, scale.height)
    gc.fillText(s"${scale.finerScale.getPostfixString(scale.dateTimeFromTimestamp(cursorTime))}",
      cursorPos, scale.height - 60)

    gc.save()
    datasetValues.foreach {
      case (dataset, Some(leaf: BTreeLeaf[FloatAggregator])) =>
        val value = leaf.point._2
        gc.setFill(dataset.color)
        gc.fillText(f"${dataset.name} = ${value}%.3g",
          cursorPos, scale.yValToPos(value))
      case (dataset, Some(aggr: BTreeAggregate[FloatAggregator])) =>
        val value = aggr.nodeData.sum / aggr.nodeData.count
        gc.setFill(dataset.color)
        gc.fillText(f"${dataset.name} = ${value}%.3g",
          cursorPos, scale.yValToPos(value))
      case (dataset, None) =>  // discard
    }
    gc.restore()
  }
}
