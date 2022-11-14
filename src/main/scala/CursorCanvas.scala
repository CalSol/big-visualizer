package bigvis

import btree.{BTreeAggregate, BTreeData, BTreeLeaf, FloatAggregator}
import control._

import scalafx.scene.paint.Color

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

      LabelGroup(newDataMinPos, newDataMaxPos, newCount, newViewMinPos, newViewMaxPos)
    }

    def overlapsWith(other: LabelGroup): Boolean = {
      !(viewMin >= other.viewMax || other.viewMin >= viewMax)
    }
  }

  /** Given a list of originalPositions, spreads them out such that they are at least minSpread apart
   * while bounded by min and max.
   *
   * Algorithm: merge positions that are less than minSpread apart, placing them minSpread apart but centered
   * at the midpoint of the lowest and highest original positions in the merge group.
   * Repeat until there are no more positions to be merged (fixed-point algorithm).
   */
  def spreadPositions(originalPositions: Seq[Double], spread: Double, min: Double, max: Double): Seq[Double] = {
    require(originalPositions.sorted == originalPositions, "TODO handle non-sorted input")

    var positions = originalPositions.map { pos =>
      LabelGroup(pos, pos, 1, pos - spread / 2, pos + spread / 2)
    }
    // Repeatedly merge
    if (positions.nonEmpty) {
      var done = false
      while (!done) {
        val newPositions = mutable.ListBuffer[LabelGroup]()
        var last = positions.head
        positions.tail.foreach { next =>
          if (last.overlapsWith(next)) {
            last = last.mergeWith(next, min, max, spread)
          } else {
            newPositions.append(last)
            last = next
          }
        }
        newPositions.append(last)

        if (newPositions == positions) {
          done = true
        }
        positions = newPositions.toSeq
      }
    }
    // Transform to individual coordinates
    val finalPositions = positions.flatMap { group =>
      (0 until group.count).map { i =>  // return center coordinates
        group.viewMin + (spread / 2) + (i * spread)
      }
    }
    require(finalPositions.length == originalPositions.length, f"${finalPositions}  ${originalPositions}")
    finalPositions
  }
}


class CursorCanvas extends BaseChartCanvas {
  import CursorCanvas._

  def draw(scale: ChartParameters, cursorPos: Double,
           datasetData: Seq[(String, BTreeData[FloatAggregator], Color)]): Unit = {
    val gc = getGraphicsContext2D

    gc.clearRect(0, 0, scale.width, scale.height)

    val cursorTime = scale.xPosToVal(cursorPos)
    gc.strokeLine(cursorPos, 0, cursorPos, scale.height)
    gc.fillText(s"${scale.finerScale.getPostfixString(scale.dateTimeFromTimestamp(cursorTime))}",
      cursorPos, scale.height - 60)

    val datasetValues = datasetData.map {
      case (name, leaf: BTreeLeaf[FloatAggregator], color) =>
        (name, leaf.point._2, color)
      case (name, aggr: BTreeAggregate[FloatAggregator], color) =>
        (name, aggr.nodeData.sum / aggr.nodeData.count, color)
    }.sortBy(-_._2)

    val originalPositions = datasetValues.map { case (name, value, color) =>
      scale.yValToPos(value)
    }
    val positions = spreadPositions(originalPositions, 12, 0, scale.height)

    gc.save()
    (datasetValues zip positions).foreach { case ((name, value, color), position) =>
        gc.setFill(color)
      RenderHelper.drawContrastText(gc, ChartCommon.CONTRAST_BACKGROUND,
          f"$name = $value%.5g",
          cursorPos, position)
    }
    gc.restore()
  }
}
