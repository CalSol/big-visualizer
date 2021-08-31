package bigvis

import bigvis.btree.{BTree, BTreeIntermediateNode, BTreeLeaf, BTreeLeafNode, BTreeNode, FloatAggregate}
import javafx.scene.input.{MouseEvent, ScrollEvent}
import scalafx.application.JFXApp
import scalafx.application.JFXApp.PrimaryStage
import scalafx.beans.property.StringProperty
import scalafx.collections.ObservableBuffer
import scalafx.scene.Scene
import scalafx.scene.chart.{LineChart, NumberAxis, XYChart}
import scalafx.scene.control.{SplitPane, TreeItem, TreeTableColumn, TreeTableView}
import scalafx.scene.layout.{Priority, StackPane, VBox}
import scalafx.scene.layout.VBox.setVgrow

import java.io.File
import collection.mutable
import com.github.tototoshi.csv.CSVReader


// TODO split into data model
case class DataItem(name: String) {
  val nameProp = StringProperty(name)
  val dataProp = StringProperty("0")
}


/**
 * VBox containing several stacked charts, with glue to make their X axes appear synchronized
 */
class SharedAxisCharts extends VBox {
  case class ContainedChart(chart: BTreeChart)

  protected val charts = mutable.ArrayBuffer[ContainedChart]()

  // Adds a chart to the end of this stack of charts, and sets the axis properties to make it do the right thing
  def addChart(chart: StackPane): Unit = {
    chart.setOnScroll((t: ScrollEvent) => {
      onScroll(t)
    })
    chart.setOnMousePressed((t: MouseEvent) => {
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

    val (newLower, newUpper) = if (event.isShiftDown) {  // shift to zoom
      val increment = -event.getDeltaX  // shifts X/Y axes: https://stackoverflow.com/questions/42429591/javafx-shiftscrollwheel-always-return-0-0

      val range = lastChart.xUpper.value - lastChart.xLower.value
      val mouseFrac = event.getX / lastChart.getWidth  // in percent of chart from left
      val mouseTime = lastChart.xLower.value + (range * mouseFrac).toLong

      val newRange = range * Math.pow(1.01, increment)
      (mouseTime - (newRange * mouseFrac).toLong, mouseTime + (newRange * (1 - mouseFrac)).toLong)
    } else {  // normal scroll, left/right
      val increment = -event.getDeltaY
      val range = lastChart.xUpper.value - lastChart.xLower.value
      val shift = (range / 256) * increment
      (lastChart.xLower.value + shift.toLong, lastChart.xUpper.value + shift.toLong)
    }

    charts.foreach(chart => {
      chart.chart.xLower.value = newLower
      chart.chart.xUpper.value = newUpper
    })
  }

  protected def onMouse(event: MouseEvent): Unit = {

  }

  def zoomMax(): Unit = {
    val minTime = charts.map(_.chart.xLower.value).min
    val maxTime = charts.map(_.chart.xUpper.value).max

    charts.foreach(chart => {
      chart.chart.xLower.value = minTime
      chart.chart.xUpper.value = maxTime
    })
  }
}


object Main extends JFXApp {
  // See layouts documentation
  // https://docs.oracle.com/javafx/2/layout/builtin_layouts.htm

  val dataRoot = new TreeItem(DataItem("root")) {
    expanded = true
    children = Seq(
      new TreeItem(DataItem("quack")),
      new TreeItem(DataItem("lol")),
      new TreeItem(DataItem("cats")),
    )
  }

  val navigationPane = new VBox {
    // See table view example at
    // https://github.com/scalafx/ScalaFX-Tutorials/blob/master/slick-table/src/main/scala/org/scalafx/slick_table/ContactsView.scala
    // and tree view example at
    // https://github.com/scalafx/scalafx/blob/master/scalafx-demos/src/main/scala/scalafx/controls/treetableview/TreeTableViewWithTwoColumns.scala
    val tree = new TreeTableView[DataItem](dataRoot) {
      columns ++= Seq(
        new TreeTableColumn[DataItem, String] {
          text = "Name"
          cellValueFactory = { _.value.value.value.nameProp }
        },
        new TreeTableColumn[DataItem, String] {
          text = "Data"
          cellValueFactory = { _.value.value.value.dataProp }
        }
      )
    }
    children = Seq(tree)
    setVgrow(tree, Priority.Always)
  }

  println("Open file")
  val batteriesTree = new BTree(FloatAggregate.aggregator, 16)
  val reader = CSVReader.open(new File("../big-analysis/Fsgp21Decode/bms.pack.voltage.csv"))
  println("Map data")
  val batteriesData = reader.toStream.tail.flatMap { fields =>  // tail to discard header
    (fields(0).toDoubleOption, fields(2).toFloatOption) match {  // we actually need the double resolution for timestamp
      case (Some(time), Some(data)) => Some(((time * 1000).toLong, data))
      case _ => None
    }
  }
  println(s"batteries data: ${batteriesData.length}")

  batteriesTree.appendAll(batteriesData)
  println(s"tree insert, h=${batteriesTree.maxDepth}")

  // TODO the wrapping doesn't belong here
  val visualizationPane = new SharedAxisCharts
  visualizationPane.addChart(new StackPane(delegate=new BTreeChart(batteriesTree, 1000)))

  visualizationPane.zoomMax()

  stage = new PrimaryStage {
    title = "Big Data Visualizer"
    scene = new Scene {
      val splitPane = new SplitPane {
        items ++= Seq(navigationPane, visualizationPane)
        SplitPane.setResizableWithParent(navigationPane, false)
//        SplitPane.setResizableWithParent(visualizationPane, false)
      }
      splitPane.setDividerPositions(0.25)
      root = splitPane
    }
  }
}
