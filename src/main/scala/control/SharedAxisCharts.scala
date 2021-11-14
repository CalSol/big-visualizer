package bigvis
package control

import btree.{BTree, FloatAggregator, StringAggregator}
import javafx.scene.input.{MouseEvent, ScrollEvent}
import scalafx.Includes._
import scalafx.scene.input.{DragEvent, TransferMode}
import scalafx.scene.layout.VBox.setVgrow
import scalafx.scene.layout.{Priority, StackPane, VBox}

import scala.collection.mutable

/**
 * VBox containing several stacked charts, with glue to make their X axes appear synchronized
 */
class SharedAxisCharts(dataItems: mutable.HashMap[String, BTreeData]) extends VBox {
  case class ContainedChart(chart: BTreeChart)

  protected val charts = mutable.ArrayBuffer[ContainedChart]()

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
            println("FloatAgg")
            addChart(new StackPane(delegate = new BTreeChart(
              Seq(ChartDefinition(bTreeData.name, tree, ChartTools.createColors(1).head)),
              1000
            )))
          case tree: BTree[StringAggregator] @unchecked if tree.aggregatorType == StringAggregator.aggregator =>
            ???
          case tree => throw new IllegalArgumentException(s"bad tree $tree of type ${tree.getClass.getName}")
        }
      }
      case _ =>  // shouldn't get here
    }
    event.consume()
  }

  //  val chartDefs = (cellTrees zip ChartTools.createColors(cellTrees.length)).zipWithIndex.map { case ((cellTree, cellColor), i) =>
  //    ChartDefinition(f"cell-$i", cellTree, cellColor)
  //  }
  //  val chartDefs = Seq()

  //  visualizationPane.addChart(new StackPane(delegate=
  //    new BTreeChart(chartDefs, 1000)))
  //  visualizationPane.zoomMax()


  // Adds a chart to the end of this stack of charts, and sets the axis properties to make it do the right thing
  def addChart(chart: StackPane): Unit = {
    chart.setOnScroll((t: ScrollEvent) => {
      onScroll(t)
    })
    chart.setOnMouseMoved((t: MouseEvent) => {
      onMouse(t)
    })

    setVgrow(chart, Priority.Always)
    children.add(chart)
    charts.append(ContainedChart(
      chart.delegate.asInstanceOf[BTreeChart],
    ))
  }

  protected def onScroll(event: ScrollEvent): Unit = {
    event.consume()

    val lastChart = charts.last.chart
    if (event.isShiftDown) {
      if (event.isControlDown) {
        val increment = -event.getDeltaX // shifts X/Y axes: https://stackoverflow.com/questions/42429591/javafx-shiftscrollwheel-always-return-0-0
        val range = lastChart.yUpper.value - lastChart.yLower.value
        val mouseFrac = 1 - event.getY / lastChart.getHeight
        val mouseValue = lastChart.yLower.value + (range * mouseFrac)
        val newRange = range * Math.pow(1.01, increment)
        charts.foreach(chart => {
          chart.chart.yLower.value = mouseValue - (newRange * mouseFrac)
          chart.chart.yUpper.value = mouseValue + (newRange * (1 - mouseFrac))
        })
      } else {
        val increment = -event.getDeltaX
        val range = lastChart.yUpper.value - lastChart.yLower.value
        val shift = (range / 256) * increment
        charts.foreach(chart => {
          chart.chart.yLower.value = lastChart.yLower.value + shift
          chart.chart.yUpper.value = lastChart.yUpper.value + shift
        })
      }
    } else {
      if (event.isControlDown) {
        val increment = -event.getDeltaY // consistent with Chrome's zoom UI

        val range = lastChart.xUpper.value - lastChart.xLower.value
        val mouseFrac = event.getX / lastChart.getWidth // in percent of chart from left
        val mouseTime = lastChart.xLower.value + (range * mouseFrac).toLong

        val newRange = range * Math.pow(1.01, increment)
        charts.foreach(chart => {
          chart.chart.xLower.value = mouseTime - (newRange * mouseFrac).toLong
          chart.chart.xUpper.value = mouseTime + (newRange * (1 - mouseFrac)).toLong
        })
      } else {
        val increment = -event.getDeltaY
        val range = lastChart.xUpper.value - lastChart.xLower.value
        val shift = (range / 256) * increment
        charts.foreach(chart => {
          chart.chart.xLower.value = lastChart.xLower.value + shift.toLong
          chart.chart.xUpper.value = lastChart.xUpper.value + shift.toLong
        })
      }
    }
  }
  protected def onMouse(event: MouseEvent): Unit = {
    charts.foreach(chart => {
      chart.chart.cursorXPos.value = event.getX
    })
  }

  def zoomMax(): Unit = {
    val minXTime = charts.map(_.chart.xLower.value).min
    val maxXTime = charts.map(_.chart.xUpper.value).max
    val minYTime = charts.map(_.chart.yLower.value).min
    val maxYTime = charts.map(_.chart.yUpper.value).max
    charts.foreach(chart => {
      chart.chart.xLower.value = minXTime
      chart.chart.xUpper.value = maxXTime
      chart.chart.yLower.value = minYTime
      chart.chart.yUpper.value = maxYTime
    })
  }
}
