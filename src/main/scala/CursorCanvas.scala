package bigvis

import btree.{BTreeAggregate, BTreeData, BTreeLeaf, FloatAggregator}


object CursorCanvas {
  /** Given a list of originalPositions, spreads them out such that they are at least minSpread apart
   * while bounded by min and max.
   */
  def spreadPositions(originalPositions: Seq[Double], minSpread: Double, min: Double, max: Double): Seq[Double] = {
    originalPositions
  }
}


class CursorCanvas extends ResizableCanvas {
  import CursorCanvas._

  def draw(scale: ChartParameters, cursorPos: Double,
           datasetData: Seq[(ChartDefinition, Option[BTreeData[FloatAggregator]])]): Unit = {
    val gc = getGraphicsContext2D

    gc.clearRect(0, 0, scale.width, scale.height)

    val cursorTime = scale.xPosToVal(cursorPos)
    gc.strokeLine(cursorPos, 0, cursorPos, scale.height)
    gc.fillText(s"${scale.finerScale.getPostfixString(scale.dateTimeFromTimestamp(cursorTime))}",
      cursorPos, scale.height - 60)

    val datasetValues = datasetData.collect {
      case (dataset, Some(leaf: BTreeLeaf[FloatAggregator])) =>
        (dataset, leaf.point._2)
      case (dataset, Some(aggr: BTreeAggregate[FloatAggregator])) =>
        (dataset, aggr.nodeData.sum / aggr.nodeData.count)
    }

    val originalPositions = datasetValues.map { case (dataset, value) =>
      scale.yValToPos(value)
    }
    val positions = spreadPositions(originalPositions, 10, 0, scale.yMax)

    gc.save()
    (datasetValues zip positions).foreach { case ((dataset, value), position) =>
        gc.setFill(dataset.color)
        gc.fillText(f"${dataset.name} = ${value}%.5g",
          cursorPos, position)
    }
    gc.restore()
  }
}
