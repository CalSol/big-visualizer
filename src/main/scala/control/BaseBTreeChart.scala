package bigvis
package control

import btree._

import scalafx.scene.paint.Color
import scalafx.scene.layout.StackPane
import scalafx.Includes._
import scalafx.beans.property.{DoubleProperty, ObjectProperty}
import scalafx.scene.input.ScrollEvent

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


case class ChartParameters(width: Int, height: Int, xAxis: (Long, Long), yAxis: (Double, Double),
                           timeZone: ZoneId) {
  val (xMin, xMax) = xAxis
  val (yMin, yMax) = yAxis

  val xRange: Long = xAxis._2 - xAxis._1
  val xScale: Double = width.toDouble / xRange  // multiply time units by this to get offset in pixels
  val yRange: Double = yAxis._2 - yAxis._1
  val yScale: Double = height.toDouble / yRange  // multiply value units by this to get offset in pixels

  // select the ticks where there is at most one tick per 64px
  // TODO parameterized
  val tickScale: AxisScale = AxisScales.getScaleWithBestSpan((64 / xScale).toLong)
  val contextScale: ContextAxisScale = AxisScales.getContextScale(tickScale)
  val finerScale: ContextAxisScale = AxisScales.getFinerScale(tickScale)

  def xValToPos(value: Long): Double = (value - xAxis._1) * xScale
  def xPosToVal(pos: Double): Long = (pos / xScale).toLong + xAxis._1
  def yValToPos(value: Double): Double = (yAxis._2 - value) * yScale

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
abstract class BaseBTreeChart(val container: SharedAxisCharts) extends StackPane {
  // implement me: add a dataset to this chart
  def addDataset(series: BTreeSeries): Boolean

  minWidth = 0  // allow resizing down
  minHeight = 0  // allow resizing down

  val yAxis: ObjectProperty[(Double, Double)] = ObjectProperty((0.0, 0.0))  // bottom (lower), upper (higher)

  this.onScroll = (event: ScrollEvent) => {
    if (event.isShiftDown) {
      if (event.isControlDown) {
        val increment = -event.getDeltaX // shifts X/Y axes: https://stackoverflow.com/questions/42429591/javafx-shiftscrollwheel-always-return-0-0
        val range = yAxis.value._2 - yAxis.value._1
        val mouseFrac = 1 - event.getY / height.value
        val mouseValue = yAxis.value._1 + (range * mouseFrac)
        val newRange = range * Math.pow(1.01, increment)
        yAxis.value = (mouseValue - (newRange * mouseFrac), mouseValue + (newRange * (1 - mouseFrac)))
      } else {
        val increment = -event.getDeltaX
        val range = yAxis.value._2 - yAxis.value._1
        val shift = (range / 256) * increment
        yAxis.value = (yAxis.value._1 + shift, yAxis.value._2 + shift)
      }
      event.consume()
    }
  }

  val gridCanvas = new GridCanvas()
  children.append(gridCanvas)
  gridCanvas.widthProperty().bind(width)
  gridCanvas.heightProperty().bind(height)
  Seq(width, height, container.xAxis).foreach { observable =>
    observable.onChange(redrawGrid())
  }

  protected def redrawGrid(): Unit = {
    val scale = ChartParameters(width.value.toInt, height.value.toInt,
      container.xAxis.value, (0, 0), container.timeZone)
    gridCanvas.draw(scale)
  }
}
