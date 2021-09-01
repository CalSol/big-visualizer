package bigvis

import bigvis.btree.{BTree, BTreeData, BTreeLeaf, BTreeNode, FloatAggregate}
import javafx.scene.canvas.Canvas
import javafx.scene.layout.StackPane
import javafx.scene.paint.Color
import scalafx.beans.property.{DoubleProperty, LongProperty}
import javafx.scene.canvas.GraphicsContext


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

  class ResizableCanvas extends Canvas {
    widthProperty.addListener(evt => draw())
    heightProperty.addListener(evt => draw())
    xLower.addListener(evt => draw())
    xUpper.addListener(evt => draw())
    yLower.addListener(evt => draw())
    yUpper.addListener(evt => draw())

    override def isResizable: Boolean = true

    def draw(): Unit = {
      val gc = getGraphicsContext2D
      val width = getWidth
      val height = getHeight

      gc.clearRect(0, 0, width, height)

      gc.fillText(s"${yLower.value}", 0, height)
      gc.fillText(s"${yUpper.value}", 0, 10)

      val xBottom = xLower.value
      val xScale = width / (xUpper.value - xLower.value)
      val yTop = yUpper.value
      val yScale = height / (yUpper.value - yLower.value)

      // select the ticks where there is at most one tick per 64px
      val tickScale = AxisScales.getScaleWithBestSpan((64 / xScale).toLong)
      val (priorTime, tickTimes) = tickScale.getTicks(xLower.value, xUpper.value)

      val contextScale = AxisScales.getContextScale(tickScale)
      val (priorContextTime, contextTimes) = contextScale.getTicks(xLower.value, xUpper.value)

      gc.save()
      gc.setStroke(gc.getStroke.asInstanceOf[Color].deriveColor(0, 1, 1, 0.25))
      // draw the context gridlines
      gc.save()
      gc.setLineWidth(2)
      contextTimes.foreach { tickTime =>
        val position = (tickTime - xBottom) * xScale
        gc.strokeLine(position, 0, position, height)
      }
      gc.restore()

      // draw the ticklines
      tickTimes.foreach { tickTime =>
        val position = (tickTime - xBottom) * xScale
        gc.strokeLine(position, 0, position, height)
      }
      gc.restore()

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

      // render the nodes
      val renderTime = timeExec {
        sections.foreach { section =>
          val sectionPoints = section.map {
            case node: BTreeNode[FloatAggregate, Float] =>
              ((node.minTime + node.maxTime) / 2, node.nodeData.sum / node.nodeData.count)
            case node: BTreeLeaf[FloatAggregate, Float] =>
              node.point
          }

          gc.strokePolyline(
            sectionPoints.map(point => (point._1 - xBottom) * xScale).toArray,
            sectionPoints.map(point => (yTop - point._2) * yScale).toArray,
            sectionPoints.length)
        }
      }

      // draw context ruler
      val background = Color.rgb(255, 255, 255, 0.75)
      gc.save()
      gc.setLineWidth(2)
      contextTimes.foreach { tickTime =>
        val position = (tickTime - xBottom) * xScale
        RenderHelper.drawContrastLine(gc, background, position, height - 20, position, height)
      }
      gc.restore()

      (xLower.value +: (contextTimes :+ xUpper.value)).sliding(2).foreach { case Seq(curr, next) =>
        val position = ((curr + next) / 2 - xBottom) * xScale
        // TODO anchor center
        RenderHelper.drawContrastText(gc, background, contextScale.getPrefixString(curr), position, height - 10)
      }

      // draw tick ruler
      tickTimes.foreach { tickTime =>
        val position = (tickTime - xBottom) * xScale
        RenderHelper.drawContrastLine(gc, background, position, height - 30, position, height - 20)
        RenderHelper.drawContrastText(gc, background, contextScale.getPostfixString(tickTime), position + 4, height - 20)
      }

      // render debugging information
      gc.fillText(s"${nodes.length} nodes, ${sections.length} sections", 0, 20)
      gc.fillText(f"${nodeTime * 1000}%.1f ms nodes, " +
          f"${sectionTime * 1000}%.1f ms sections, " +
          f"${renderTime * 1000}%.1f ms render",
        0, 30)
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
