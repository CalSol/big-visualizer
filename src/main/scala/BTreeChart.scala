package bigvis

import btree._

import javafx.scene.canvas.{Canvas, GraphicsContext}
import javafx.scene.layout.StackPane
import javafx.scene.paint.Color
import scalafx.beans.property.{DoubleProperty, LongProperty}

import java.time.{Instant, ZoneId, ZoneOffset, ZonedDateTime}
import scala.collection.Searching.{Found, InsertionPoint}
import scala.collection.mutable


object RenderHelper {
  def drawContrastLine(gc: GraphicsContext, background: Color,
                       x1: Double, y1: Double, x2: Double, y2: Double): Unit = {
    gc.save()
    gc.setStroke(background)
    gc.setLineWidth(gc.getLineWidth * 5)
    gc.strokeLine(x1, y1, x2, y2)
    gc.restore()
    gc.strokeLine(x1, y1, x2, y2)
  }

  def drawContrastText(gc: GraphicsContext, background: Color, text: String,
                       x: Double, y: Double): Unit = {
    gc.save()
    gc.setStroke(background)
    gc.setLineWidth(5)
    gc.strokeText(text, x, y)
    gc.restore()
    gc.fillText(text, x, y)
  }
}


case class ChartDefinition(
    name: String,
    data: BTree[FloatAggregator],
    color: Color
)


case class ChartMetadata(
                      nodeTime: Double,
                      sectionTime: Double,
                      resampleTime: Double,
                      nodes: Long,
                      resampledNodes: Long
                      )


object ChartTools {
  // Using the golden ratio method to generate chart colors, from
  // https://martin.ankerl.com/2009/12/09/how-to-create-random-colors-programmatically/
  // https://softwareengineering.stackexchange.com/questions/198065/what-algorithms-are-there-for-picking-colors-for-plot-lines-on-graphs
  // TODO also mess with saturation and value?
  protected val goldenRatioConjugate = 0.618033988749895

  def createColors(count: Int): Seq[Color] = {
    (0 until count).map { i =>
      Color.hsb((goldenRatioConjugate * 360 * i) % 360, 0.75, 0.75, 0.5)
    }
  }
}


case class ChartParameters(width: Int, height: Int, xMin: Long, xMax: Long, yMin: Double, yMax: Double,
                           timeZone: ZoneId) {
  val xRange: Long = xMax - xMin
  val xScale: Double = width.toDouble / xRange  // multiply time units by this to get offset in pixels
  val yRange: Double = yMax - yMin
  val yScale: Double = height.toDouble / yRange  // multiply value units by this to get offset in pixels

  // select the ticks where there is at most one tick per 64px
  // TODO parameterized
  val tickScale: AxisScale = AxisScales.getScaleWithBestSpan((64 / xScale).toLong)
  val contextScale: ContextAxisScale = AxisScales.getContextScale(tickScale)
  val finerScale: ContextAxisScale = AxisScales.getFinerScale(tickScale)

  def xValToPos(value: Long): Double = (value - xMin) * xScale
  def xPosToVal(pos: Double): Long = (pos / xScale).toLong + xMin
  def yValToPos(value: Double): Double = (yMax - value) * yScale

  // TODO is this the right place for these functions to live?
  def timestampFromDateTime(dateTime: ZonedDateTime): Long = {
    val utcDateTime = dateTime.withZoneSameInstant(ZoneOffset.UTC)
    utcDateTime.toEpochSecond * 1000 + utcDateTime.getNano / 1000 / 1000
  }

  def dateTimeFromTimestamp(timestamp: Long): ZonedDateTime = {
    val utcDateTime = ZonedDateTime.ofInstant(Instant.ofEpochMilli(timestamp), ZoneOffset.UTC)
    utcDateTime.withZoneSameInstant(timeZone)
  }
}


class ResizableCanvas extends Canvas {
  override def isResizable: Boolean = true
}


// A JavaFX widget that does lean and mean plotting without the CSS bloat that kills performance
// Inspired by:
// charting: https://dlsc.com/2015/06/16/javafx-tip-20-a-lot-to-show-use-canvas/
// custom controls: https://stackoverflow.com/questions/43808639/how-to-create-totally-custom-javafx-control-or-how-to-create-pane-with-dynamic
class BTreeChart(datasets: Seq[ChartDefinition], timeBreak: Long) extends StackPane {
  val xLower: LongProperty = LongProperty(datasets.map(_.data.minTime).min)
  val xUpper: LongProperty = LongProperty(datasets.map(_.data.maxTime).max)

  val yLower: DoubleProperty = DoubleProperty(datasets.map(_.data.rootData.min).min)
  val yUpper: DoubleProperty = DoubleProperty(datasets.map(_.data.rootData.max).max)

  val cursorXPos: DoubleProperty = DoubleProperty(Double.NaN)  // in screen units

  // Processed data displayed by the current window
  val windowSections: mutable.HashMap[String, Seq[Seq[BTreeData[FloatAggregator]]]] = mutable.HashMap()


  // Given a set of parameters (defining the window and resolution) and a data series (BTree),
  // returns the sectioned (broken by timeBreak if below the minimum resolution) and resampled data.
  def getData(scale: ChartParameters, series: BTree[FloatAggregator]):
      (Seq[Seq[BTreeData[FloatAggregator]]], ChartMetadata) = {
    val minResolution = scale.xRange / scale.width

    val (nodeTime, nodes) = timeExec {
      // TODO
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

    val (resampleTime, sections) = timeExec {
      rawSections.map { rawSection =>
        BTreeResampler(FloatAggregator.aggregator, rawSection, minResolution)
      }
    }

    val chartMetadata = ChartMetadata(nodeTime, sectionTime, resampleTime,
      nodes.length, sections.map(_.length).sum)

    (sections, chartMetadata)
  }

  setMinWidth(0)  // alow resizing down
  setMinHeight(0)  // alow resizing down

  val gridCanvas = new GridCanvas()
  getChildren.add(gridCanvas)
  gridCanvas.widthProperty().bind(widthProperty())
  gridCanvas.heightProperty().bind(heightProperty())

  val chartCanvas = new ChartCanvas()
  getChildren.add(chartCanvas)
  chartCanvas.widthProperty().bind(widthProperty())
  chartCanvas.heightProperty().bind(heightProperty())

  val cursorCanvas = new CursorCanvas()
  getChildren.add(cursorCanvas)
  cursorCanvas.widthProperty().bind(widthProperty())
  cursorCanvas.heightProperty().bind(heightProperty())

  widthProperty.addListener(_ => redrawFromGrid())
  heightProperty.addListener(_ => redrawFromGrid())
  xLower.addListener(_ => redrawFromGrid())
  xUpper.addListener(_ => redrawFromGrid())

  yLower.addListener(_ => redrawFromChart())
  yUpper.addListener(_ => redrawFromChart())

  cursorXPos.addListener(_ => redrawFromCursor())

  val timeZone = ZoneId.of(ZoneId.SHORT_IDS.get("CST"))  // TODO user-configurable

  def redrawFromGrid(): Unit = {
    // TODO can we dedup some of these?
    val scale = ChartParameters(getWidth.toInt, getHeight.toInt,
      xLower.value, xUpper.value, yLower.value, yUpper.value, timeZone)
    redrawGrid(scale)
    redrawChart(scale)
    redrawCursor(scale)
  }

  def redrawFromChart(): Unit = {
    // TODO can we dedup some of these?
    val scale = ChartParameters(getWidth.toInt, getHeight.toInt,
      xLower.value, xUpper.value, yLower.value, yUpper.value, timeZone)
    redrawChart(scale)
    redrawCursor(scale)
  }

  def redrawFromCursor(): Unit = {
    // TODO can we dedup some of these?
    val scale = ChartParameters(getWidth.toInt, getHeight.toInt,
      xLower.value, xUpper.value, yLower.value, yUpper.value, timeZone)
    redrawCursor(scale)
  }

  protected def redrawGrid(scale: ChartParameters): Unit = {
    gridCanvas.draw(scale)
  }

  // Refresh the windowSections 'cache' and redraw the chart
  protected def redrawChart(scale: ChartParameters): Unit = {
    windowSections.clear()
    val charts = datasets.map { dataset =>
      val (sections, chartMetadata) = getData(scale, dataset.data)
      windowSections.put(dataset.name, sections)
      (dataset, chartMetadata, sections)
    }

    chartCanvas.draw(scale, charts)
  }

  protected def redrawCursor(scale: ChartParameters): Unit = {
    val cursorPos = cursorXPos.value
    val cursorTime = scale.xPosToVal(cursorPos)

    // TODO get rod of the null
    val searchPoint: BTreeData[FloatAggregator] =
      new BTreeResampledNode[FloatAggregator](cursorTime, cursorTime, null)

    def dataToStartTime(data: BTreeData[FloatAggregator]): Long = data match {
      case leaf: BTreeLeaf[FloatAggregator] => leaf.point._1
      case aggr: BTreeAggregate[FloatAggregator] => aggr.minTime
    }

    val datasetValues = datasets.map { dataset =>
      val data = windowSections.get(dataset.name).map { sections =>
        // Find the section containing the requested time point
        val section = sections.search(Seq(searchPoint))(Ordering.by(e => dataToStartTime(e.head))) match {
          case Found(foundIndex) => sections(foundIndex)
          case InsertionPoint(insertionPoint) => sections(insertionPoint)  // TODO more robust =o
        }
        // Then find the data point within the section
        val data = section.search(searchPoint)(Ordering.by(dataToStartTime)) match {
          case Found(foundIndex) => section(foundIndex)
          case InsertionPoint(insertionPoint) => section(insertionPoint)
        }
        data
      }
      // TODO discard option None case here
      (dataset, data)
    }
    cursorCanvas.draw(scale, cursorPos, datasetValues)
  }
}
