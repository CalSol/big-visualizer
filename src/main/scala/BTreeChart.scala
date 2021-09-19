package bigvis

import bigvis.btree.{BTree, BTreeData, BTreeLeaf, BTreeNode, FloatAggregator}
import javafx.scene.SnapshotParameters
import javafx.scene.canvas.Canvas
import javafx.scene.layout.StackPane
import javafx.scene.paint.Color
import scalafx.beans.property.{DoubleProperty, LongProperty}
import javafx.scene.canvas.GraphicsContext
import javafx.scene.image.WritableImage

import java.time.{Instant, ZoneId, ZoneOffset, ZonedDateTime}
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


case class ChartParameters(width: Int, height: Int, xMin: Long, xMax: Long, yMin: Double, yMax: Double) {
  val xRange = xMax - xMin
  val xScale = width.toDouble / xRange  // multiply time units by this to get offset in pixels
  val yRange = yMax - yMin
  val yScale = height.toDouble / yRange  // multiply value units by this to get offset in pixels

  // select the ticks where there is at most one tick per 64px
  // TODO parameterized
  val tickScale = AxisScales.getScaleWithBestSpan((64 / xScale).toLong)
  val contextScale = AxisScales.getContextScale(tickScale)

  def xValToPos(value: Double): Double = (value - xMin) * xScale
  def xPosToVal(pos: Double): Long = (pos / xScale).toLong + xMin
  def yValToPos(value: Double): Double = (yMax - value) * yScale
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

  // Rendering properties
  protected val GRIDLINE_ALPHA = 0.25
  protected val CONTEXT_GRIDLINE_WIDTH = 2

  protected val CONTRAST_BACKGROUND = Color.rgb(255, 255, 255, 0.75)

  protected val AGGREGATE_ALPHA = 0.33

  def timestampFromDateTime(dateTime: ZonedDateTime): Long = {
    val utcDateTime = dateTime.withZoneSameInstant(ZoneOffset.UTC)
    utcDateTime.toEpochSecond * 1000 + utcDateTime.getNano / 1000 / 1000
  }

  def dateTimeFromTimestamp(timestamp: Long): ZonedDateTime = {
    val utcDateTime = ZonedDateTime.ofInstant(Instant.ofEpochMilli(timestamp), ZoneOffset.UTC)
    utcDateTime.withZoneSameInstant(ZoneId.of(ZoneId.SHORT_IDS.get("CST")))
  }

  class ResizableCanvas extends Canvas {
    widthProperty.addListener(evt => redrawAll())
    heightProperty.addListener(evt => redrawAll())
    xLower.addListener(evt => redrawAll())
    xUpper.addListener(evt => redrawAll())
    yLower.addListener(evt => redrawChart())
    yUpper.addListener(evt => redrawChart())
    cursorXPos.addListener(evt => redrawCursor())

    override def isResizable: Boolean = true

    // Saved graphics to speed up rendering
    protected var timeGridImage: Option[WritableImage] = None
    protected var chartImage: Option[WritableImage] = None

    protected def redrawAll(): Unit = {  // invalidates everything and redraw
      timeGridImage = None
      chartImage = None
      draw()
    }
    protected def redrawChart(): Unit = {  // invalidates the cached chart (but keeps the time grid)
      chartImage = None
      draw()
    }
    protected def redrawCursor(): Unit = {  // invalidates nothing (only redraws cursor)
      draw()
    }

    // Actual rendering functions
    protected def drawGridlines(gc: GraphicsContext, scale: ChartParameters,
                                tickTimes: Seq[ZonedDateTime], contextTimes: Seq[ZonedDateTime]): Unit = {
      gc.save()
      gc.setStroke(gc.getStroke.asInstanceOf[Color].deriveColor(0, 1, 1, GRIDLINE_ALPHA))

      // draw the context gridlines
      gc.save()
      gc.setLineWidth(CONTEXT_GRIDLINE_WIDTH)
      contextTimes.foreach { tickTime =>
        val position = scale.xValToPos(timestampFromDateTime(tickTime))
        gc.strokeLine(position, 0, position, scale.height)
      }
      gc.restore()

      // draw the ticklines
      tickTimes.foreach { tickTime =>
        val position = scale.xValToPos(timestampFromDateTime(tickTime))
        gc.strokeLine(position, 0, position, scale.height)
      }

      gc.restore()
    }

    protected def drawRulers(gc: GraphicsContext, scale: ChartParameters,
                             priorContextTime: ZonedDateTime,
                             tickTimes: Seq[ZonedDateTime], contextTimes: Seq[ZonedDateTime]): Unit = {
      gc.save()
      gc.setLineWidth(CONTEXT_GRIDLINE_WIDTH)
      contextTimes.foreach { tickTime =>
        val position = scale.xValToPos(timestampFromDateTime(tickTime))
        RenderHelper.drawContrastLine(gc, CONTRAST_BACKGROUND,
          position, scale.height - 20,
          position, scale.height)
      }
      gc.restore()

      // for positioning the fenceposts
      val paddedContextPositions = scale.xMin +: (contextTimes.map(timestampFromDateTime) :+ scale.xMax)
      // for the actual labels - this goes between the fenceposts so has one less entry
      val paddedContextLabels = priorContextTime +: contextTimes

      (paddedContextPositions.sliding(2) zip paddedContextLabels).foreach { case (Seq(currPos, nextPos), label) =>
        val position = scale.xValToPos((currPos + nextPos) / 2)
        // TODO anchor center
        RenderHelper.drawContrastText(gc, CONTRAST_BACKGROUND, scale.contextScale.getPrefixString(label),
          position, scale.height - 10)
      }

      // draw tick ruler
      tickTimes.foreach { tickTime =>
        val position = scale.xValToPos(timestampFromDateTime(tickTime))
        RenderHelper.drawContrastLine(gc, CONTRAST_BACKGROUND,
          position, scale.height - 30,
          position, scale.height - 20)
        RenderHelper.drawContrastText(gc, CONTRAST_BACKGROUND, scale.tickScale.getPostfixString(tickTime),
          position + 4, scale.height - 20)
      }
    }

    protected def drawChart(gc: GraphicsContext, scale: ChartParameters, series: BTree[FloatAggregator], chartColor: Color, offset: Int): Unit = {
      val (nodeTime, nodes) = timeExec {
        // TODO
        series.getData(scale.xMin, scale.xMax, (scale.xRange.toDouble / scale.width).toLong)
      }

      // filter nodes into break-able sections
      val (sectionTime, sections) = timeExec {
        ChunkSeq(nodes, scale.xMin, (prevTime: Long, elem: BTreeData[FloatAggregator]) => {
          elem match {
            case node: BTreeNode[FloatAggregator] =>
              (node.maxTime, node.minTime > prevTime + timeBreak)
            case node: BTreeLeaf[FloatAggregator] => // TODO return individual data points
              (node.point._1, node.point._1 > prevTime + timeBreak)
          }
        })
      }

      gc.save()
      gc.setFill(chartColor)
      gc.setStroke(chartColor)

      val renderTime = timeExec {
        sections.foreach { section =>
          // render the aggregate ranges
          val contiguousNodeSubsections = ChunkSeq[BTreeData[FloatAggregator], BTreeData[FloatAggregator]](section, section.head, {
            case (prev: BTreeNode[FloatAggregator], curr: BTreeNode[FloatAggregator]) => (curr, false)
            case (prev: BTreeLeaf[FloatAggregator], curr: BTreeLeaf[FloatAggregator]) => (curr, false)
            case (_, curr) => (curr, true)
          })
          gc.save()
          gc.setFill(chartColor.deriveColor(0, 1, 1, AGGREGATE_ALPHA))
          contiguousNodeSubsections
              .filter(_.head.isInstanceOf[BTreeNode[FloatAggregator]])
              .asInstanceOf[Seq[Seq[BTreeNode[FloatAggregator]]]]
              .foreach { subsection =>
                val bottomPoints = subsection.map { node =>
                  ((node.maxTime + node.minTime) / 2, node.nodeData.min)
                }
                val topPoints = subsection.map { node =>
                  ((node.maxTime + node.minTime) / 2, node.nodeData.max)
                }
                val polygonPoints = bottomPoints ++ topPoints.reverse
                val polygonXs = polygonPoints.map{point => scale.xValToPos(point._1)}.toArray
                val polygonYs = polygonPoints.map{point => scale.yValToPos(point._2)}.toArray
                  gc.fillPolygon(polygonXs, polygonYs, polygonPoints.size)
              }
          gc.restore()

          // render the data / average lines
          val sectionPoints = section.map {
            case node: BTreeNode[FloatAggregator] =>
              ((node.minTime + node.maxTime) / 2, node.nodeData.sum / node.nodeData.count)
            case node: BTreeLeaf[FloatAggregator] =>
              // TODO only render at some density instead of by B-tree?
              gc.fillOval(
                scale.xValToPos(node.point._1) - 2,
                scale.yValToPos(node.point._2) - 2,
                4, 4)  // render as points
              node.point
          }

          gc.strokePolyline(
            sectionPoints.map(point => scale.xValToPos(point._1)).toArray,
            sectionPoints.map(point => scale.yValToPos(point._2)).toArray,
            sectionPoints.length)
        }
      }

      // render debugging information
      gc.fillText(f"${(nodeTime + sectionTime + renderTime) * 1000}%.1f ms total    " +
          f"${nodeTime * 1000}%.1f ms nodes, " +
          f"${sectionTime * 1000}%.1f ms sections, " +
          f"${renderTime * 1000}%.1f ms render    " +
          f"${nodes.length} nodes, ${sections.length} sections",
        0, 20 + (offset * 10))

      gc.restore()
    }

    protected def drawCursor(gc: GraphicsContext, scale: ChartParameters): Unit = {
      val cursorPos = cursorXPos.value
      val cursorTime = scale.xPosToVal(cursorPos)
      gc.strokeLine(cursorPos, 0, cursorPos, scale.height)
      gc.strokeText(s"${cursorTime}", cursorPos, scale.height - 60)
    }

    protected def draw(): Unit = {
      val gc = getGraphicsContext2D
      val scale = ChartParameters(getWidth.toInt, getHeight.toInt,
        xLower.value, xUpper.value, yLower.value, yUpper.value)

      gc.clearRect(0, 0, scale.width, scale.height)

      // actually draw everything
      timeGridImage match {
        case Some(timeGridImage) =>
          gc.drawImage(timeGridImage, 0, 0)
        case None => // redraw the time grid
          val (priorTime, tickTimes) = scale.tickScale.getTicks(
            dateTimeFromTimestamp(scale.xMin), dateTimeFromTimestamp(scale.xMax))
          val (priorContextTime, contextTimes) = scale.contextScale.getTicks(
            dateTimeFromTimestamp(scale.xMin), dateTimeFromTimestamp(scale.xMax))

          drawGridlines(gc, scale, tickTimes, contextTimes)
          drawRulers(gc, scale, priorContextTime, tickTimes, contextTimes)
          timeGridImage = Some(canvas.snapshot(new SnapshotParameters, null))
      }

      chartImage match {
        case Some(chartImage) =>
          gc.drawImage(chartImage, 0, 0)
        case None =>
          // TODO proper scales for Y axis
          gc.fillText(s"${scale.yMax}", 0, scale.height)
          gc.fillText(s"${scale.yMax}", 0, 10)

          val renderTime = timeExec {
            datasets.zipWithIndex.foreach { case (dataset, i) =>
              drawChart(gc, scale, dataset.data, dataset.color, i)
            }
            chartImage = Some(canvas.snapshot(new SnapshotParameters, null))
          }
          gc.fillText(f" => ${renderTime * 1000}%.1f ms total render",
            0, 20 + (datasets.length * 10) + 10)
      }

      drawCursor(gc, scale)
    }
  }

  val canvas = new ResizableCanvas()

  getChildren.add(canvas)
  canvas.widthProperty().bind(widthProperty())
  canvas.heightProperty().bind(heightProperty())
}
