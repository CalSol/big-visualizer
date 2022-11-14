package bigvis
package control

import btree._

import javafx.scene.paint.Color
import scalafx.collections.ObservableBuffer

import scala.collection.mutable


case class FloatBTreeSeries(name: String, tree: BTree[FloatAggregator], color: Color)


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
class FloatBTreeChart(parent: SharedAxisCharts, timeBreak: Long)
    extends BaseBTreeChart(parent) with XYBTreeChart {
  import BTreeChart._

  protected val datasets = ObservableBuffer[FloatBTreeSeries]()

  override def addDataset(dataset: BTreeSeries): Boolean = {
    if (dataset.tree.aggregatorType != FloatAggregator.aggregator) {
      return false
    }
    val tree = dataset.tree.asInstanceOf[BTree[FloatAggregator]]
    if (datasets.isEmpty) {
      yLower.value = tree.rootData.min
      yUpper.value = tree.rootData.max
    }

    datasets.append(FloatBTreeSeries(
      dataset.name, tree,
      ChartTools.colorForIndex(datasets.length)
    ))

    true
  }

  // Processed data displayed by the current window
  val windowSections: mutable.HashMap[String, SectionedData[FloatAggregator]] = mutable.HashMap()

  // Given a set of parameters (defining the window and resolution) and a data series (BTree),
  // returns the sectioned (broken by timeBreak if below the minimum resolution) and resampled data.
  def getData(scale: ChartParameters, series: BTree[FloatAggregator], name: String): SectionedData[FloatAggregator] = {
    val minResolution = (scale.xRange.toDouble / scale.width * PX_PER_POINT).toLong

    val (nodeTime, nodes) = timeExec {
      series.getData(scale.xMin, scale.xMax, minResolution)
    }

    // resample (combine) nodes to reduce the resolution to nearer the minimum
    val (resampleTime, resampledNodes) = timeExec {
      BTreeResampler(FloatAggregator.aggregator, nodes, minResolution)
    }

    // filter nodes into break-able sections
    val (sectionTime, sectionedData) = timeExec {
      SectionedData.from(resampledNodes, scale.xMin, timeBreak)
    }

    PerfTreeView().foreach(_.updateItemPerf(name,
      nodes.length, resampledNodes.length, nodeTime, sectionTime, resampleTime
    ))

    sectionedData
  }

  val chartCanvas = new SectionedFloatChartCanvas()
  children.append(chartCanvas)
  chartCanvas.widthProperty().bind(width)
  chartCanvas.heightProperty().bind(height)
  Seq(width, height, parent.xLower, parent.xUpper, yLower, yUpper, datasets).foreach { observable =>
    observable.onInvalidate(redrawChart())
  }

  val cursorCanvas = new CursorCanvas()
  children.append(cursorCanvas)
  cursorCanvas.widthProperty().bind(width)
  cursorCanvas.heightProperty().bind(height)
  Seq(width, height, parent.xLower, parent.xUpper, yLower, yUpper, parent.cursorXPos, datasets).foreach { observable =>
    observable.onInvalidate(redrawCursor())
  }

  // Refresh the windowSections 'cache' and redraw the chart
  protected def redrawChart(): Unit = {
    val scale = ChartParameters(width.value.toInt, height.value.toInt,
      parent.xLower.value, parent.xUpper.value, yLower.value, yUpper.value, timeZone)

    windowSections.clear()
    val charts = datasets.map { dataset =>
      val sectionedData = getData(scale, dataset.tree, dataset.name)
      windowSections.put(dataset.name, sectionedData)

      (dataset, sectionedData.sections)
    }

    chartCanvas.draw(scale, charts.toSeq)
  }

  protected def redrawCursor(): Unit = {
    val scale = ChartParameters(width.value.toInt, height.value.toInt,
      parent.xLower.value, parent.xUpper.value, yLower.value, yUpper.value, timeZone)
    val tolerance = (ChartCommon.CURSOR_SNAP_PX / scale.xScale).toLong

    val cursorPos = parent.cursorXPos.value
    val cursorTime = scale.xPosToVal(cursorPos)

    val datasetValues = datasets.flatMap { dataset =>
      windowSections(dataset.name).getClosestValue(cursorTime, tolerance)
          .map(dataset -> _)
    }
    cursorCanvas.draw(scale, cursorPos, datasetValues.toSeq)
  }
}
