package bigvis
package control

import btree.BTree

import scalafx.Includes._
import scalafx.beans.property.{DoubleProperty, ObjectProperty}
import scalafx.geometry.Orientation
import scalafx.scene.control.SplitPane
import scalafx.scene.input.{DragEvent, MouseEvent, ScrollEvent, TransferMode}
import scalafx.scene.layout.Priority
import scalafx.scene.layout.VBox.setVgrow

import java.time.ZoneId
import scala.collection.mutable


/**
 * VBox containing several stacked charts, with glue to make their X axes appear synchronized
 */
class SharedAxisCharts(val dataItems: mutable.HashMap[String, BTreeSeries]) extends SplitPane {
  orientation = Orientation.Vertical

  val xAxis: ObjectProperty[(BTree.TimestampType, BTree.TimestampType)] = ObjectProperty((0L, 0L))  // lower, upper
  val cursorXPos: DoubleProperty = DoubleProperty(Double.NaN)  // in screen units

  def timeZone: ZoneId = ZoneId.of(ZoneId.SHORT_IDS.get("CST"))  // TODO user-configurable

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
        if (xAxis.value._1 == xAxis.value._2) {
          xAxis.value = (bTreeData.tree.minTime, bTreeData.tree.maxTime)
        }
        PerfTreeView().foreach(_.addItem(bTreeData.name))
        val chart = BTreeChart.fromTree(this, bTreeData)
        setVgrow(chart, Priority.Always)
        this.items.add(new DraggableBTreeChartWrapper(chart))
      }
      case _ =>  // shouldn't get here
    }
    event.consume()
  }

  this.onScroll = (event: ScrollEvent) => {
    if (event.isControlDown) {
      val increment = -event.getDeltaY // consistent with Chrome's zoom UI

      val range = xAxis.value._2 - xAxis.value._1
      val mouseFrac = event.getX / width.value // in percent of chart from left
      val mouseTime = xAxis.value._1 + (range * mouseFrac).toLong
      val newRange = range * Math.pow(1.01, increment)

      xAxis.value = (mouseTime - (newRange * mouseFrac).toLong, mouseTime + (newRange * (1 - mouseFrac)).toLong)
    } else {
      val increment = -event.getDeltaY
      val range = xAxis.value._2 - xAxis.value._1
      val shift = (range / 256) * increment

      xAxis.value = (xAxis.value._1 + shift.toLong, xAxis.value._2 + shift.toLong)
    }
    event.consume()
  }

  this.onMouseMoved = (event: MouseEvent) => {
    cursorXPos.value = event.getX
  }
}
