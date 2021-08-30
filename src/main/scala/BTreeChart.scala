package bigvis

import bigvis.btree.{BTree, FloatAggregate}
import javafx.scene.canvas.Canvas
import javafx.scene.layout.StackPane
import scalafx.beans.property.{DoubleProperty, LongProperty}

import scala.collection.mutable


object ChunkSeq {
  // Splits arrays based on some logic applied for each pair
  def apply[ElemType, DataType](seq: Seq[ElemType], initVal: DataType,
                                fn: (ElemType, ElemType, DataType) => (DataType, Boolean)): Seq[Seq[ElemType]] = {
    val outputBuilder = mutable.ArrayBuffer[Seq[ElemType]]()
    val elemBuilder = mutable.ArrayBuffer[ElemType]()
    if (seq.isEmpty) {
      Seq()
    } else {
      var prevElem = seq.head
      var prevData = initVal
      elemBuilder.append(prevElem)
      seq.tail.foreach { elem =>
        val (newData, split) = fn(prevElem, elem, prevData)
        if (split) {
          outputBuilder.append(elemBuilder.toSeq)
          elemBuilder.clear()
          elemBuilder.append(elem)
        } else {
          elemBuilder.append(elem)
        }
        prevElem = elem
        prevData = newData
      }
      outputBuilder.append(elemBuilder.toSeq)
    }
    outputBuilder.toSeq
  }
}


// A JavaFX widget that does lean and mean plotting without the CSS bloat that kills performance
// Inspired by:
// charting: https://dlsc.com/2015/06/16/javafx-tip-20-a-lot-to-show-use-canvas/
// custom controls: https://stackoverflow.com/questions/43808639/how-to-create-totally-custom-javafx-control-or-how-to-create-pane-with-dynamic
class BTreeChart(data: BTree[FloatAggregate, Float], timeBreak: Long) extends StackPane {
  class ResizableCanvas extends Canvas {
    widthProperty.addListener(evt => draw())
    heightProperty.addListener(evt => draw())

    val xLower = LongProperty(data.minTime)
    val xUpper = LongProperty(data.maxTime)

    val yLower = DoubleProperty(data.rootData.min)
    val yUpper = DoubleProperty(data.rootData.max)

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
      val nodes = data.getData(xLower.value, xUpper.value, (range / width).toLong)
      gc.fillText(s"${nodes.length} nodes", 0, 20)

      // filter nodes into break-able sections
//      val sections =
//      gc.strokePolyline()
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
