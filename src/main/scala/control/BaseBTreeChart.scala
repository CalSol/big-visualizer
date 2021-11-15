package bigvis
package control

import btree._

import javafx.scene.paint.Color
import scalafx.Includes._
import scalafx.scene.input.{DragEvent, TransferMode}
import scalafx.scene.layout.StackPane
import scalafx.scene.shape.Rectangle

import java.time.{Instant, ZoneId, ZoneOffset, ZonedDateTime}


object ChartCommon {
  val CONTRAST_BACKGROUND: Color = Color.rgb(255, 255, 255, 0.75)
  val CURSOR_SNAP_PX = 16
}


object ChartTools {
  // Using the golden ratio method to generate chart colors, from
  // https://martin.ankerl.com/2009/12/09/how-to-create-random-colors-programmatically/
  // https://softwareengineering.stackexchange.com/questions/198065/what-algorithms-are-there-for-picking-colors-for-plot-lines-on-graphs
  // TODO also mess with saturation and value?
  protected val goldenRatioConjugate = 0.618033988749895

  def colorForIndex(index: Int): Color = {
    Color.hsb((goldenRatioConjugate * 360 * index) % 360, 0.75, 0.75, 0.5)
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


object BTreeChart {  // rendering properties
  val PX_PER_POINT = 3.0  // minimum pixels between points - higher increases performance at cost of 'sharpness'

  def fromTree(parent: SharedAxisCharts, series: BTreeSeries): BaseBTreeChart = series.tree match {
    // TODO figure out a clean way around type erasure
    case tree: BTree[FloatAggregator] @unchecked if tree.aggregatorType == FloatAggregator.aggregator =>
      val chart = new FloatBTreeChart(parent, 1000)  // TODO customizable timeBreak
      chart.addDataset(series)
      chart
    case tree: BTree[StringAggregator] @unchecked if tree.aggregatorType == StringAggregator.aggregator =>
      ???
    case tree => throw new IllegalArgumentException(s"bad tree $tree of type ${tree.getClass.getName}")
  }
}


// Base BTreeChart class that provides time axis functionality.
abstract class BaseBTreeChart(parent: SharedAxisCharts) extends StackPane {
  minWidth = 0  // allow resizing down
  minHeight = 0  // allow resizing down


  protected val dragRect = Rectangle(0, 20, width.value, height.value - 40)
  dragRect.setFill(Color.TRANSPARENT)
  Seq(width, height).foreach { observable =>
    observable.onChange{
      dragRect.setWidth(width.value)
      dragRect.setHeight(height.value - 40)
    }
  }

  dragRect.onDragOver = (event: DragEvent) => {
    event.dragboard.content.get(DataTreeView.BTreeDataType) match {
      case Some(str: String) =>
        event.acceptTransferModes(TransferMode.Copy)
        dragRect.setFill(Color.BLUE.deriveColor(0, 1, 1, 0.5))
      case _ =>
        dragRect.setFill(Color.RED.deriveColor(0, 1, 1, 0.5))
    }
    event.consume()
  }
  dragRect.onDragExited = (event: DragEvent) => {
    dragRect.setFill(Color.TRANSPARENT)
    event.consume()
  }
  dragRect.onDragDropped = (event: DragEvent) => {
    event.dragboard.content.get(DataTreeView.BTreeDataType) match {
      case Some(str: String) => parent.dataItems.get(str).foreach { bTreeData =>
        addDataset(bTreeData)
        dragRect.setFill(Color.TRANSPARENT)
      }
      case _ => // shouldn't get here
    }
    event.consume()
  }

  // Adds a dataset, returning whether it was successfully added
  def addDataset(series: BTreeSeries): Boolean

  val chartsPane = new StackPane {
    minWidth = 0
    minHeight = 0
  }
  children.append(chartsPane)
  children.append(dragRect)  // must be on top

  val gridCanvas = new GridCanvas()
  chartsPane.children.append(gridCanvas)
  gridCanvas.widthProperty().bind(width)
  gridCanvas.heightProperty().bind(height)
  Seq(width, height, parent.xLower, parent.xUpper).foreach { observable =>
    observable.onChange(redrawGrid())
  }

  protected def timeZone: ZoneId = ZoneId.of(ZoneId.SHORT_IDS.get("CST"))  // TODO user-configurable

  protected def redrawGrid(): Unit = {
    val scale = ChartParameters(width.value.toInt, height.value.toInt,
      parent.xLower.value, parent.xUpper.value, 0, 0, timeZone)
    gridCanvas.draw(scale)
  }
}
