package bigvis

import bigvis.btree.{BTree, BTreeData, BTreeLeaf, BTreeNode, FloatAggregate}
import javafx.scene.canvas.Canvas
import javafx.scene.layout.StackPane
import scalafx.beans.property.{DoubleProperty, LongProperty}


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

      gc.fillText(s"${xLower.value} / ${yLower.value}", 0, height)
      gc.fillText(s"${yUpper.value}", 0, 10)
      gc.fillText(s"${xUpper.value}", width - 100, height)  // TODO anchor right

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

      val xBottom = xLower.value
      val xScale = width / (xUpper.value - xLower.value)
      val yTop = yUpper.value
      val yScale = height / (yUpper.value - yLower.value)

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
