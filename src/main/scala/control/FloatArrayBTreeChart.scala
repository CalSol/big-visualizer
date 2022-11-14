package bigvis
package control

import btree._

import javafx.scene.paint.Color
import scalafx.collections.ObservableBuffer

import scala.collection.mutable


case class FloatArrayBTreeSeries(name: String, tree: BTree[FloatArrayAggregator], color: Color)


class FloatArrayBTreeChart(parent: SharedAxisCharts, timeBreak: Long)
    extends BaseBTreeChart(parent) {
  import BTreeChart._

  protected val datasets = ObservableBuffer[FloatArrayBTreeSeries]()

  override def addDataset(dataset: BTreeSeries): Boolean = {
    if (dataset.tree.aggregatorType != FloatArrayAggregator.aggregator) {
      return false
    }
    val tree = dataset.tree.asInstanceOf[BTree[FloatArrayAggregator]]
    if (datasets.isEmpty) {
      yAxis.value = (tree.rootData.min, tree.rootData.max)
    }

    datasets.append(FloatArrayBTreeSeries(
      dataset.name, tree,
      ChartTools.colorForIndex(datasets.length)
    ))

    true
  }

  // Processed data displayed by the current window
  val windowSections: mutable.HashMap[String, IndexedSeq[IndexedSeq[BTreeData[FloatArrayAggregator]]]] = mutable.HashMap()

  // Given a set of parameters (defining the window and resolution) and a data series (BTree),
  // returns the sectioned (broken by timeBreak if below the minimum resolution) and resampled data.
  def getData(scale: ChartParameters, series: BTree[FloatAggregator]):
  (IndexedSeq[IndexedSeq[BTreeData[FloatAggregator]]], ChartMetadata) = {
    val minResolution = (scale.xRange.toDouble / scale.width * PX_PER_POINT).toLong

    val (nodeTime, nodes) = timeExec {
      series.getData(scale.xMin, scale.xMax, minResolution)
    }

    // filter nodes into break-able sections
    val (sectionTime, rawSections) = timeExec {
      ChunkSeq(nodes, scale.xMin, (prevTime: Long, elem: BTreeData[FloatAggregator]) => {
        elem match {
          case node: BTreeAggregate[FloatAggregator] =>
            (node.maxTime, node.minTime > prevTime + timeBreak)
          case node: BTreeLeaf[FloatAggregator] => // TODO return individual data points
            (node.point._1, node.point._1 > prevTime + timeBreak)
        }
      })
    }

    val (resampleTime, sections) = timeExec {  // TODO create IndexedSeqs earlier?
      rawSections.map { rawSection =>
        BTreeResampler(FloatAggregator.aggregator, rawSection, minResolution).toIndexedSeq
      }.toIndexedSeq
    }

    val chartMetadata = ChartMetadata(nodeTime, sectionTime, resampleTime,
      nodes.length, sections.map(_.length).sum)

    (sections, chartMetadata)
  }

//  val chartCanvas = new ChartCanvas()
//  children.append(chartCanvas)
//  chartCanvas.widthProperty().bind(width)
//  chartCanvas.heightProperty().bind(height)
//  Seq(width, height, parent.xLower, parent.xUpper, yLower, yUpper, datasets).foreach { observable =>
//    observable.onInvalidate(redrawChart())
//  }
//
//  val cursorCanvas = new CursorCanvas()
//  children.append(cursorCanvas)
//  cursorCanvas.widthProperty().bind(width)
//  cursorCanvas.heightProperty().bind(height)
//  Seq(width, height, parent.xLower, parent.xUpper, yLower, yUpper, parent.cursorXPos, datasets).foreach { observable =>
//    observable.onInvalidate(redrawCursor())
//  }
//
//  // Refresh the windowSections 'cache' and redraw the chart
//  protected def redrawChart(): Unit = {
//    val scale = ChartParameters(width.value.toInt, height.value.toInt,
//      parent.xLower.value, parent.xUpper.value, yLower.value, yUpper.value, timeZone)
//
//    windowSections.clear()
//    val charts = datasets.map { dataset =>
//      val (sections, perf) = getData(scale, dataset.tree)
//      windowSections.put(dataset.name, sections)
//
//      PerfTreeView().foreach(_.updateItemPerf(dataset.name,
//        perf.nodes, perf.resampledNodes, perf.nodeTime, perf.sectionTime, perf.resampleTime
//      ))
//      (dataset, sections)
//    }
//
//    chartCanvas.draw(scale, charts.toSeq)
//  }
//
//  protected def redrawCursor(): Unit = {
//    val scale = ChartParameters(width.value.toInt, height.value.toInt,
//      parent.xLower.value, parent.xUpper.value, yLower.value, yUpper.value, timeZone)
//
//    val cursorPos = parent.cursorXPos.value
//    val cursorTime = scale.xPosToVal(cursorPos)
//
//    // TODO get rod of the null
//    val searchPoint: BTreeData[FloatAggregator] =
//      new BTreeResampledNode[FloatAggregator](cursorTime, cursorTime, null)
//
//    def dataToStartTime(data: BTreeData[FloatAggregator]): Long = data match {
//      case leaf: BTreeLeaf[FloatAggregator] => leaf.point._1
//      case aggr: BTreeAggregate[FloatAggregator] => aggr.minTime
//    }
//    def dataToEndTime(data: BTreeData[FloatAggregator]): Long = data match {
//      case leaf: BTreeLeaf[FloatAggregator] => leaf.point._1
//      case aggr: BTreeAggregate[FloatAggregator] => aggr.maxTime
//    }
//
//    val datasetValues = datasets.map { dataset =>
//      val data = windowSections.get(dataset.name).flatMap { sections =>
//        // Find the section nearest the requested time point
//        val sectionsIntervals = sections.map { section =>
//          (dataToStartTime(section.head), dataToEndTime(section.last))
//        }
//        SearchInterval(sectionsIntervals, cursorTime) match {
//          case Some(result: SearchInterval.SearchIntervalResult[Long]) => Some(sections(result.index()))
//          case None => None
//        }
//      }.flatMap { section =>
//        // Then find the data point within the section
//        val sectionIntervals = section.map { node =>
//          (dataToStartTime(node), dataToEndTime(node))
//        }
//        val tolerance = ChartCommon.CURSOR_SNAP_PX / scale.xScale
//        SearchInterval(sectionIntervals, cursorTime) match {
//          case Some(SearchInterval.ContainedIn(index)) =>
//            Some(section(index))
//          case Some(SearchInterval.NearestBefore(index, distance)) if distance <= tolerance =>
//            Some(section(index))
//          case Some(SearchInterval.NearestAfter(index, distance)) if distance <= tolerance =>
//            Some(section(index))
//          case _ => None
//        }
//      }
//      // TODO discard option None case here
//      (dataset, data)
//    }
//    cursorCanvas.draw(scale, cursorPos, datasetValues.toSeq)
//  }
}
