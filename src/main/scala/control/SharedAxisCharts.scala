package bigvis
package control

import btree.{BTree, FloatAggregator, StringAggregator}

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
class SharedAxisCharts(dataItems: mutable.HashMap[String, BTreeData]) extends SplitPane {
  orientation = Orientation.Vertical

  val xLower: LongProperty = LongProperty(0)
  val xUpper: LongProperty = LongProperty(0)
  val cursorXPos: DoubleProperty = DoubleProperty(Double.NaN)  // in screen units


  // Returns children as charts
  protected def charts: Seq[BTreeChart] = this.items.collect {
    case chart: BTreeChart => chart
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
        bTreeData.tree match {
          // TODO figure out a clean way around type erasure
          case tree: BTree[FloatAggregator] @unchecked if tree.aggregatorType == FloatAggregator.aggregator =>
            addChart(bTreeData.name, tree)
          case tree: BTree[StringAggregator] @unchecked if tree.aggregatorType == StringAggregator.aggregator =>
            ???
          case tree => throw new IllegalArgumentException(s"bad tree $tree of type ${tree.getClass.getName}")
        }
      }
      case _ =>  // shouldn't get here
    }
    event.consume()
  }

  // Adds a chart to the end of this stack of charts, and sets the axis properties to make it do the right thing
  def addChart(name: String, tree: BTree[FloatAggregator]): Unit = {
    if (xLower.value == xUpper.value) {
      xLower.value = tree.minTime
      xUpper.value = tree.maxTime
    }

    val chart = new BTreeChart(
      this,
      Seq(ChartDefinition(name, tree, ChartTools.createColors(1).head)),
      1000
    )
    setVgrow(chart, Priority.Always)
    this.items.add(chart)
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
