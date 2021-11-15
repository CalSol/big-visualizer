package bigvis
package control

import scalafx.Includes._
import scalafx.beans.property.{DoubleProperty, LongProperty}
import scalafx.geometry.Orientation
import scalafx.scene.control.SplitPane
import scalafx.scene.input.{DragEvent, MouseEvent, ScrollEvent, TransferMode}
import scalafx.scene.layout.Priority
import scalafx.scene.layout.VBox.setVgrow

import scala.collection.mutable


/**
 * VBox containing several stacked charts, with glue to make their X axes appear synchronized
 */
class SharedAxisCharts(val dataItems: mutable.HashMap[String, BTreeSeries]) extends SplitPane {
  orientation = Orientation.Vertical

  val xLower: LongProperty = LongProperty(0)
  val xUpper: LongProperty = LongProperty(0)
  val cursorXPos: DoubleProperty = DoubleProperty(Double.NaN)  // in screen units


  // Returns children as charts
  protected def charts: Seq[BaseBTreeChart] = this.items.collect {
    case chart: BaseBTreeChart => chart
  }.toSeq

  this.onDragOver = (event: DragEvent) => {
    event.dragboard.content.get(DataTreeView.BTreeDataType) match {
      case Some(str: String) =>
        event.acceptTransferModes(TransferMode.Copy)
      case _ =>
    }
    event.consume()
  }

  this.onDragDropped = (event: DragEvent) => {
    event.dragboard.content.get(DataTreeView.BTreeDataType) match {
      case Some(str: String) => dataItems.get(str).foreach { bTreeData =>
        if (xLower.value == xUpper.value) {
          xLower.value = bTreeData.tree.minTime
          xUpper.value = bTreeData.tree.maxTime
        }
        val chart = BTreeChart.fromTree(this, bTreeData)
        setVgrow(chart, Priority.Always)
        this.items.add(chart)
      }
      case _ =>  // shouldn't get here
    }
    event.consume()
  }

  this.onScroll = (event: ScrollEvent) => {
    if (event.isControlDown) {
      val increment = -event.getDeltaY // consistent with Chrome's zoom UI

      val range = xUpper.value - xLower.value
      val mouseFrac = event.getX / width.value // in percent of chart from left
      val mouseTime = xLower.value + (range * mouseFrac).toLong
      val newRange = range * Math.pow(1.01, increment)

      xLower.value = mouseTime - (newRange * mouseFrac).toLong
      xUpper.value = mouseTime + (newRange * (1 - mouseFrac)).toLong
    } else {
      val increment = -event.getDeltaY
      val range = xUpper.value - xLower.value
      val shift = (range / 256) * increment

      xLower.value = xLower.value + shift.toLong
      xUpper.value = xUpper.value + shift.toLong
    }
    event.consume()
  }

  this.onMouseMoved = (event: MouseEvent) => {
    cursorXPos.value = event.getX
  }
}
