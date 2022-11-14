package bigvis
package control

import btree.BTree.TimestampType
import btree._

import scalafx.scene.paint.Color


class FloatArrayBTreeChart(parent: SharedAxisCharts, val timeBreak: Long)
    extends BaseBTreeChart(parent) with MultiDatasetBTreeChart[FloatArrayAggregator]
        with CachedSectionedMultiDatasetBTreeChart[FloatArrayAggregator]
        with CursorBTreeChart {
  override protected val aggregatorType: FloatArrayAggregator = FloatArrayAggregator.aggregator
  override protected def getTreeValueLimits(tree: BTree[FloatArrayAggregator]): (Double, Double) = {
    (tree.rootData.min, tree.rootData.max)
  }
  override def getCursorData(scale: ChartParameters, xPos: TimestampType): Seq[(String, Double, Color)] = {
    val tolerance = (ChartCommon.CURSOR_SNAP_PX / scale.xScale).toLong

    datasets.toSeq.flatMap { case (name, (tree, color)) =>
      cachedSections(name).getClosestValue(xPos, tolerance).map {
        case leaf: BTreeLeaf[FloatArrayAggregator] =>
          // TODO show all individual points
          val value = leaf.point._2.sum / leaf.point._2.length
          (f"$name = $value%.5g", value, color)
        case aggr: BTreeAggregate[FloatArrayAggregator] =>
          val average = aggr.nodeData.sum / aggr.nodeData.count
          (f"$name = ($average%.5g)", average, color)
      }
    }
  }


  val chartCanvas = new SectionedFloatChartCanvas()
  children.append(chartCanvas)
  chartCanvas.widthProperty().bind(width)
  chartCanvas.heightProperty().bind(height)
  cachedSections.onChange(redrawChart())

  cachedSections.onChange(redrawCursor())
  children.append(cursorCanvas)

  protected def redrawChart(): Unit = {
    val scale = ChartParameters(width.value.toInt, height.value.toInt,
      container.xAxis.value, yAxis.value, container.timeZone)
    val charts = datasets.map { case (name, (tree, color)) =>
      (name, cachedSections(name), color)
    }
//    chartCanvas.draw(scale, charts.toSeq)
  }
}
