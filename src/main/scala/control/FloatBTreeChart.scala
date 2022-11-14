package bigvis
package control

import btree._

import bigvis.btree.BTree.TimestampType
import scalafx.scene.paint.Color


// A JavaFX widget that does lean and mean plotting without the CSS bloat that kills performance
// Inspired by:
// charting: https://dlsc.com/2015/06/16/javafx-tip-20-a-lot-to-show-use-canvas/
// custom controls: https://stackoverflow.com/questions/43808639/how-to-create-totally-custom-javafx-control-or-how-to-create-pane-with-dynamic
class FloatBTreeChart(parent: SharedAxisCharts, val timeBreak: Long)
    extends BaseBTreeChart(parent) with MultiDatasetBTreeChart[FloatAggregator]
        with CachedSectionedMultiDatasetBTreeChart[FloatAggregator]
        with CursorBTreeChart {
  override protected val aggregatorType: FloatAggregator = FloatAggregator.aggregator
  override protected def getTreeValueLimits(tree: BTree[FloatAggregator]): (Double, Double) = {
    (tree.rootData.min, tree.rootData.max)
  }
  override def getCursorData(scale: ChartParameters, xPos: TimestampType): Seq[(String, Double, Color)] = {
    val tolerance = (ChartCommon.CURSOR_SNAP_PX / scale.xScale).toLong

    datasets.toSeq.flatMap { case (name, (tree, color)) =>
      cachedSections(name).getClosestValue(xPos, tolerance).map {
        case leaf: BTreeLeaf[FloatAggregator] =>
          val value = leaf.point._2
          (f"$name = $value%.5g", value, color)
        case aggr: BTreeAggregate[FloatAggregator] =>
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
    chartCanvas.draw(scale, charts.toSeq)
  }
}
