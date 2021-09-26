package bigvis

import btree._

import javafx.scene.canvas.{Canvas, GraphicsContext}
import javafx.scene.layout.StackPane
import javafx.scene.paint.Color
import scalafx.beans.property.{DoubleProperty, LongProperty}

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


case class ChartParameters(width: Int, height: Int, xMin: Long, xMax: Long, yMin: Double, yMax: Double) {
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

  class GridCanvas extends ResizableCanvas {
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

    def draw(scale: ChartParameters): Unit = {
      val gc = getGraphicsContext2D

      gc.clearRect(0, 0, scale.width, scale.height)

      val (priorTime, tickTimes) = scale.tickScale.getTicks(
        dateTimeFromTimestamp(scale.xMin), dateTimeFromTimestamp(scale.xMax))
      val (priorContextTime, contextTimes) = scale.contextScale.getTicks(
        dateTimeFromTimestamp(scale.xMin), dateTimeFromTimestamp(scale.xMax))

      drawGridlines(gc, scale, tickTimes, contextTimes)
      drawRulers(gc, scale, priorContextTime, tickTimes, contextTimes)

    }
  }

  class ChartCanvas extends ResizableCanvas {
    // Actual rendering functions
    protected def drawChart(gc: GraphicsContext, scale: ChartParameters,
                            sections: Seq[Seq[BTreeData[FloatAggregator]]], chartColor: Color,
                            chartMetadata: ChartMetadata,
                            offset: Int): Unit = {
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
//                  gc.fillPolygon(polygonXs, polygonYs, polygonPoints.size)
              }
          gc.restore()

          // render the data / average lines
          val sectionPoints = section.map {
            case node: BTreeAggregate[FloatAggregator] =>
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

      val totalTime = chartMetadata.nodeTime + chartMetadata.sectionTime + chartMetadata.resampleTime + renderTime
      // render debugging information
      gc.fillText(f"${totalTime * 1000}%.1f ms total    " +
          f"${chartMetadata.nodeTime * 1000}%.1f ms nodes, " +
          f"${chartMetadata.sectionTime * 1000}%.1f ms sections, " +
          f"${chartMetadata.resampleTime * 1000}%.1f ms resample, " +
          f"${renderTime * 1000}%.1f ms render    " +
          f"${chartMetadata.nodes} -> ${chartMetadata.resampledNodes} nodes",
        0, 20 + (offset * 10))

      gc.restore()
    }

    def draw(scale: ChartParameters,
             charts: Seq[(ChartDefinition, ChartMetadata, Seq[Seq[BTreeData[FloatAggregator]]])]): Unit = {
      val gc = getGraphicsContext2D

      gc.clearRect(0, 0, scale.width, scale.height)

      // TODO proper scales for Y axis
      gc.fillText(s"${scale.yMax}", 0, scale.height)
      gc.fillText(s"${scale.yMax}", 0, 10)

      val renderTime = timeExec {
        charts.zipWithIndex.foreach { case ((dataset, metadata, sections), i) =>
          drawChart(gc, scale, sections, dataset.color, metadata, i)
        }
      }
      gc.fillText(f" => ${renderTime * 1000}%.1f ms total render",
        0, 20 + (datasets.length * 10) + 10)
    }
  }


  class CursorCanvas extends ResizableCanvas {
    def draw(scale: ChartParameters, cursorPos: Double): Unit = {
      val gc = getGraphicsContext2D

      gc.clearRect(0, 0, scale.width, scale.height)

      val cursorTime = scale.xPosToVal(cursorPos)
      gc.strokeLine(cursorPos, 0, cursorPos, scale.height)
      gc.strokeText(s"${scale.finerScale.getPostfixString(dateTimeFromTimestamp(cursorTime))}",
        cursorPos, scale.height - 60)
    }
  }

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

  def redrawFromGrid(): Unit = {
    // TODO can we dedup some of these?
    val scale = ChartParameters(getWidth.toInt, getHeight.toInt,
      xLower.value, xUpper.value, yLower.value, yUpper.value)
    redrawGrid(scale)
    redrawChart(scale)
    redrawGrid(scale)
  }

  def redrawFromChart(): Unit = {
    // TODO can we dedup some of these?
    val scale = ChartParameters(getWidth.toInt, getHeight.toInt,
      xLower.value, xUpper.value, yLower.value, yUpper.value)
    redrawChart(scale)
    redrawCursor(scale)
  }

  def redrawFromCursor(): Unit = {
    // TODO can we dedup some of these?
    val scale = ChartParameters(getWidth.toInt, getHeight.toInt,
      xLower.value, xUpper.value, yLower.value, yUpper.value)
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
    cursorCanvas.draw(scale, cursorXPos.value)
  }
}
