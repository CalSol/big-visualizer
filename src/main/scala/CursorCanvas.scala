package bigvis

import btree.{BTreeAggregate, BTreeData, BTreeLeaf, FloatAggregator}

import scala.collection.mutable


object CursorCanvas {
  /** A data object representing groups of labels, with the bounds of the data they represent.
   *
   * @param dataMin, dataMax bounds considering only the data, not accounting for label size
   * @param count number of labels in this group
   * @param viewMin, viewMax bounds considering label size
   */
  case class LabelGroup(dataMin: Double, dataMax: Double, count: Int,
                        viewMin: Double, viewMax: Double) {
    def mergeWith(other: LabelGroup, limitMin: Double, limitMax: Double, spread: Double): LabelGroup = {
      val newDataMinPos = math.min(dataMin, other.dataMin)
      val newDataMaxPos = math.max(dataMax, other.dataMax)
      val newCount = count + other.count
      val halfSize = newCount * spread / 2
      val viewSize = limitMax - limitMin
      val newCenter = (newDataMinPos + newDataMaxPos) / 2
      val (newViewMinPos, newViewMaxPos) = if (halfSize * 2 > viewSize) {
        // area larger than limits - center everything because it's the only way
        val limitCenter = (limitMin + limitMax) / 2
        (limitCenter - halfSize, limitCenter + halfSize)
      } else if (newCenter - halfSize < limitMin) {  // otherwise, we only need to clip lower or upper (but not both)
        (limitMin, limitMin + halfSize * 2)
      } else if (newCenter + halfSize > limitMax) {
        (limitMax - halfSize * 2, limitMax)
      } else {
        (newCenter - halfSize, newCenter + halfSize)
      }

      LabelGroup(newDataMinPos, newDataMaxPos, count + newCount, newViewMinPos, newViewMaxPos)
    }

    def overlapsWith(other: LabelGroup): Boolean = {

    }
  }

  /** Given a list of originalPositions, spreads them out such that they are at least minSpread apart
   * while bounded by min and max.
   *
   * Algorithm: merge positions that are less than minSpread apart, placing them minSpread apart but centered
   * at the midpoint of the lowest and highest original positions in the merge group.
   * Repeat until there are no more positions to be merged (fixed-point algorithm).
   */
  def spreadPositions(originalPositions: Seq[Double], minSpread: Double, min: Double, max: Double): Seq[Double] = {
    var positions = originalPositions.map { pos =>
      LabelGroup(pos, pos, 1, pos, pos)
    }
    // Repeatedly merge
    if (positions.nonEmpty) {
      val newPositions = mutable.ListBuffer[LabelGroup]()
      var done = false
      while (!done) {
        var last = positions.head
        positions.tail.foreach { next =>

        }
      }
    }
    // Transform to individual coordinates
    positions
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
