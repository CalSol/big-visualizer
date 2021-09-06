package bigvis

import bigvis.btree.{BTree, BTreeData, BTreeLeaf, BTreeNode, FloatAggregate}
import javafx.scene.canvas.Canvas
import javafx.scene.layout.StackPane
import javafx.scene.paint.Color
import scalafx.beans.property.{DoubleProperty, LongProperty}
import javafx.scene.canvas.GraphicsContext

import java.time.{Instant, ZoneId, ZoneOffset, ZonedDateTime}


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


// A JavaFX widget that does lean and mean plotting without the CSS bloat that kills performance
// Inspired by:
// charting: https://dlsc.com/2015/06/16/javafx-tip-20-a-lot-to-show-use-canvas/
// custom controls: https://stackoverflow.com/questions/43808639/how-to-create-totally-custom-javafx-control-or-how-to-create-pane-with-dynamic
class BTreeChart(data: BTree[FloatAggregate, Float], timeBreak: Long) extends StackPane {
  val xLower: LongProperty = LongProperty(data.minTime)
  val xUpper: LongProperty = LongProperty(data.maxTime)

  val yLower: DoubleProperty = DoubleProperty(data.rootData.min)
  val yUpper: DoubleProperty = DoubleProperty(data.rootData.max)

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
    widthProperty.addListener(evt => draw())
    heightProperty.addListener(evt => draw())
    xLower.addListener(evt => draw())
    xUpper.addListener(evt => draw())
    yLower.addListener(evt => draw())
    yUpper.addListener(evt => draw())

    override def isResizable: Boolean = true

    protected def drawGridlines(gc: GraphicsContext,
                                contextTimes: Seq[ZonedDateTime], tickTimes: Seq[ZonedDateTime]): Unit = {
      // TODO can we dedup this block?
      val width = getWidth
      val height = getHeight
      val xBottom = xLower.value
      val xScale = width / (xUpper.value - xLower.value)
      val yTop = yUpper.value
      val yScale = height / (yUpper.value - yLower.value)

      gc.save()
      gc.setStroke(gc.getStroke.asInstanceOf[Color].deriveColor(0, 1, 1, GRIDLINE_ALPHA))

      // draw the context gridlines
      gc.save()
      gc.setLineWidth(CONTEXT_GRIDLINE_WIDTH)
      contextTimes.foreach { tickTime =>
        val position = (timestampFromDateTime(tickTime) - xBottom) * xScale
        gc.strokeLine(position, 0, position, height)
      }
      gc.restore()

      // draw the ticklines
      tickTimes.foreach { tickTime =>
        val position = (timestampFromDateTime(tickTime) - xBottom) * xScale
        gc.strokeLine(position, 0, position, height)
      }

      gc.restore()
    }

    protected def drawRulers(gc: GraphicsContext,
                             contextScale: ContextAxisScale, tickScale: AxisScale,
                             priorContextTime: ZonedDateTime, contextTimes: Seq[ZonedDateTime],
                             tickTimes: Seq[ZonedDateTime]): Unit = {
      // TODO can we dedup this block?
      val width = getWidth
      val height = getHeight
      val xBottom = xLower.value
      val xScale = width / (xUpper.value - xLower.value)
      val yTop = yUpper.value
      val yScale = height / (yUpper.value - yLower.value)

      gc.save()
      gc.setLineWidth(CONTEXT_GRIDLINE_WIDTH)
      contextTimes.foreach { tickTime =>
        val position = (timestampFromDateTime(tickTime) - xBottom) * xScale
        RenderHelper.drawContrastLine(gc, CONTRAST_BACKGROUND, position, height - 20, position, height)
      }
      gc.restore()

      // for positioning the fenceposts
      val paddedContextPositions = xLower.value +: (contextTimes.map(timestampFromDateTime) :+ xUpper.value)
      // for the actual labels - this goes between the fenceposts so has one less entry
      val paddedContextLabels = priorContextTime +: contextTimes

      (paddedContextPositions.sliding(2) zip paddedContextLabels).foreach { case (Seq(currPos, nextPos), label) =>
        val position = ((currPos + nextPos) / 2 - xBottom) * xScale
        // TODO anchor center
        RenderHelper.drawContrastText(gc, CONTRAST_BACKGROUND, contextScale.getPrefixString(label), position, height - 10)
      }

      // draw tick ruler
      tickTimes.foreach { tickTime =>
        val position = (timestampFromDateTime(tickTime) - xBottom) * xScale
        RenderHelper.drawContrastLine(gc, CONTRAST_BACKGROUND, position, height - 30, position, height - 20)
        RenderHelper.drawContrastText(gc, CONTRAST_BACKGROUND, tickScale.getPostfixString(tickTime), position + 4, height - 20)
      }
    }

    protected def drawChart(gc: GraphicsContext): Unit = {
      // TODO can we dedup this block?
      val width = getWidth
      val height = getHeight
      val xBottom = xLower.value
      val xScale = width / (xUpper.value - xLower.value)
      val yTop = yUpper.value
      val yScale = height / (yUpper.value - yLower.value)

      // get nodes for the current level of resolution
      val range = xUpper.value - xLower.value
      val (nodeTime, nodes) = timeExec {
        data.getData(xLower.value, xUpper.value, (range / width).toLong)
      }

      // filter nodes into break-able sections
      val (sectionTime, sections) = timeExec {
        ChunkSeq(nodes, xLower.value, (prevTime: Long, elem: BTreeData[FloatAggregate, Float]) => {
          elem match {
            case node: BTreeNode[FloatAggregate, Float] =>
              (node.maxTime, node.minTime > prevTime + timeBreak)
            case node: BTreeLeaf[FloatAggregate, Float] => // TODO return individual data points
              (node.point._1, node.point._1 > prevTime + timeBreak)
          }
        })
      }


      val renderTime = timeExec {
        sections.foreach { section =>
          // render the aggregate ranges
          val contiguousNodeSubsections = ChunkSeq[BTreeData[FloatAggregate, Float], BTreeData[FloatAggregate, Float]](section, section.head, {
            case (prev: BTreeNode[FloatAggregate, Float], curr: BTreeNode[FloatAggregate, float]) => (curr, false)
            case (prev: BTreeLeaf[FloatAggregate, Float], curr: BTreeLeaf[FloatAggregate, float]) => (curr, false)
            case (_, curr) => (curr, true)
          })
          gc.save()
          gc.setFill(gc.getFill.asInstanceOf[Color].deriveColor(0, 1, 1, AGGREGATE_ALPHA))
          contiguousNodeSubsections
              .filter(_.head.isInstanceOf[BTreeNode[FloatAggregate, Float]])
              .asInstanceOf[Seq[Seq[BTreeNode[FloatAggregate, Float]]]]
              .foreach { subsection =>
                val bottomPoints = subsection.map { node =>
                  ((node.maxTime + node.minTime) / 2, node.nodeData.min)
                }
                val topPoints = subsection.map { node =>
                  ((node.maxTime + node.minTime) / 2, node.nodeData.max)
                }
                val polygonPoints = bottomPoints ++ topPoints.reverse
                val polygonXs = polygonPoints.map{point => (point._1 - xBottom) * xScale}.toArray
                val polygonYs = polygonPoints.map{point => (yTop - point._2) * yScale}.toArray
                  gc.fillPolygon(polygonXs, polygonYs, polygonPoints.size)
              }
          gc.restore()

          // render the data / average lines
          val sectionPoints = section.map {
            case node: BTreeNode[FloatAggregate, Float] =>
              ((node.minTime + node.maxTime) / 2, node.nodeData.sum / node.nodeData.count)
            case node: BTreeLeaf[FloatAggregate, Float] =>
              // TODO only render at some density instead of by B-tree?
              gc.fillOval(
                (node.point._1 - xBottom) * xScale - 2,
                (yTop - node.point._2) * yScale - 2,
                4, 4)  // render as points
              node.point
          }

          gc.strokePolyline(
            sectionPoints.map(point => (point._1 - xBottom) * xScale).toArray,
            sectionPoints.map(point => (yTop - point._2) * yScale).toArray,
            sectionPoints.length)
        }
      }

      // render debugging information
      gc.fillText(s"${nodes.length} nodes, ${sections.length} sections", 0, 20)
      gc.fillText(f"${nodeTime * 1000}%.1f ms nodes, " +
          f"${sectionTime * 1000}%.1f ms sections, " +
          f"${renderTime * 1000}%.1f ms render",
        0, 30)
    }

    def draw(): Unit = {
      val gc = getGraphicsContext2D
      val width = getWidth
      val height = getHeight

      gc.clearRect(0, 0, width, height)

      // TODO proper scales for Y axis
      gc.fillText(s"${yLower.value}", 0, height)
      gc.fillText(s"${yUpper.value}", 0, 10)

      val xBottom = xLower.value
      val xScale = width / (xUpper.value - xLower.value)
      val yTop = yUpper.value
      val yScale = height / (yUpper.value - yLower.value)

      // select the ticks where there is at most one tick per 64px
      val tickScale = AxisScales.getScaleWithBestSpan((64 / xScale).toLong)
      val (priorTime, tickTimes) = tickScale.getTicks(
        dateTimeFromTimestamp(xLower.value), dateTimeFromTimestamp(xUpper.value))

      val contextScale = AxisScales.getContextScale(tickScale)
      val (priorContextTime, contextTimes) = contextScale.getTicks(
        dateTimeFromTimestamp(xLower.value), dateTimeFromTimestamp(xUpper.value))

      // actually draw everything
      drawGridlines(gc, contextTimes, tickTimes)

      drawChart(gc)

      drawRulers(gc, contextScale, tickScale, priorContextTime, contextTimes, tickTimes)
    }
  }

  val canvas = new ResizableCanvas()
  getChildren.add(canvas)
  canvas.widthProperty().bind(widthProperty())
  canvas.heightProperty().bind(heightProperty())

  override protected def layoutChildren(): Unit = {
    canvas.draw()
  }
}
