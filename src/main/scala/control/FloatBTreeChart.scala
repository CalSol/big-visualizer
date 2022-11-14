package bigvis
package control

import btree._


case class ChartMetadata(
                            nodeTime: Double,
                            sectionTime: Double,
                            resampleTime: Double,
                            nodes: Long,
                            resampledNodes: Long
                        )


// A JavaFX widget that does lean and mean plotting without the CSS bloat that kills performance
// Inspired by:
// charting: https://dlsc.com/2015/06/16/javafx-tip-20-a-lot-to-show-use-canvas/
// custom controls: https://stackoverflow.com/questions/43808639/how-to-create-totally-custom-javafx-control-or-how-to-create-pane-with-dynamic
class FloatBTreeChart(parent: SharedAxisCharts, val timeBreak: Long)
    extends BaseBTreeChart(parent) with MultiDatasetBTreeChart[FloatAggregator]
        with CachedSectionedMultiDatasetBTreeChart[FloatAggregator] {
  override protected val aggregatorType: FloatAggregator = FloatAggregator.aggregator
  override protected def getTreeValueLimits(tree: BTree[FloatAggregator]): (Double, Double) = {
    (tree.rootData.min, tree.rootData.max)
  }


  val chartCanvas = new SectionedFloatChartCanvas()
  children.append(chartCanvas)
  chartCanvas.widthProperty().bind(width)
  chartCanvas.heightProperty().bind(height)
  cachedSections.onChange(
    redrawChart()
  )

  val cursorCanvas = new CursorCanvas()
  children.append(cursorCanvas)
  cursorCanvas.widthProperty().bind(width)
  cursorCanvas.heightProperty().bind(height)
  Seq(cachedSections, container.cursorXPos).foreach { observable => observable.onInvalidate(
    redrawCursor()
  )}

  protected def redrawChart(): Unit = {
    val scale = ChartParameters(width.value.toInt, height.value.toInt,
      container.xAxis.value, yAxis.value, container.timeZone)
    val charts = datasets.map { case (name, (tree, color)) =>
      (name, cachedSections(name), color)
    }
    chartCanvas.draw(scale, charts.toSeq)
  }

  protected def redrawCursor(): Unit = {
    val scale = ChartParameters(width.value.toInt, height.value.toInt,
      parent.xAxis.value, yAxis.value, container.timeZone)
    val tolerance = (ChartCommon.CURSOR_SNAP_PX / scale.xScale).toLong

    val cursorPos = parent.cursorXPos.value
    val cursorTime = scale.xPosToVal(cursorPos)

    val datasetValues = datasets.flatMap { case (name, (tree, color)) =>
      cachedSections(name).getClosestValue(cursorTime, tolerance)
          .map((name, _, color))
    }
    cursorCanvas.draw(scale, cursorPos, datasetValues.toSeq)
  }
}
