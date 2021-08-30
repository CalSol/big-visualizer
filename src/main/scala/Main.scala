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
import scalafx.scene.canvas.Canvas

// TODO split into data model
case class DataItem(name: String) {
  val nameProp = StringProperty(name)
  val dataProp = StringProperty("0")
}

object ChartUpdator {
  type ChartXAxis = Number  // TODO dedup w/ SharedAxisCharts

  def updateChart(chart: XYChart[ChartXAxis, Number], data: BTree[FloatAggregate, Float]): Unit = {
    val xAxis = chart.getXAxis.asInstanceOf[javafx.scene.chart.NumberAxis]
    val lower = xAxis.getLowerBound
    val upper = xAxis.getUpperBound
    val range = upper - lower
    val widthPixels = chart.width.value

    val nodes = data.getData(lower.toLong, upper.toLong, (range / widthPixels).toLong)

    println(s"Update: $lower -> $upper (${nodes.length} nodes)")

    val dataBuffer = ObservableBuffer(nodes flatMap {
      case node: BTreeNode[FloatAggregate, Float] => Seq(
        XYChart.Data[Number, Number]((node.minTime + node.maxTime) / 2, node.nodeData.sum / node.nodeData.count)
      )
      case node: BTreeLeaf[FloatAggregate, Float] => node.leaves.map { case (time, value) =>
        XYChart.Data[Number, Number](time, value)
      }

    })
    val series = XYChart.Series[Number, Number]("test2", dataBuffer)
    chart.setData(ObservableBuffer(series))
  }
}

/**
 * VBox containing several stacked charts, with glue to make their X axes appear synchronized
 */
class SharedAxisCharts extends VBox {
  type ChartXAxis = Number

  case class ContainedChart(chart: XYChart[ChartXAxis, Number],
                            timeAxis: javafx.scene.chart.NumberAxis,
                            data: BTree[FloatAggregate, Float]
                           )

  protected val charts = mutable.ArrayBuffer[ContainedChart]()

  // Adds a chart to the end of this stack of charts, and sets the axis properties to make it do the right thing
  def addChart(chart: XYChart[Number, Number], data: BTree[FloatAggregate, Float]): Unit = {
    chart.setLegendVisible(false)

    chart.getXAxis.setAnimated(false)
    chart.getXAxis.setAutoRanging(false)

    chart.getXAxis.setVisible(false)
    chart.getXAxis.setTickMarkVisible(false)
    chart.getXAxis.setTickLabelsVisible(false)

    chart.getYAxis.setTickLabelRotation(270)  // to keep the chart size the same so stacked charts are consistent

    chart.setOnScroll((t: ScrollEvent) => {
      onScroll(t)
    })
    chart.setOnMousePressed((t: MouseEvent) => {
      onMouse(t)
    })

    setVgrow(chart, Priority.Always)
    children.add(chart)
    charts.append(ContainedChart(
      chart,
      chart.getXAxis.asInstanceOf[javafx.scene.chart.NumberAxis],  // TODO other axes types?
      data
    ))
  }

  protected def onScroll(event: ScrollEvent): Unit = {
    event.consume()

    val lastAxis = charts.last.timeAxis

    val (newLower, newUpper) = if (event.isShiftDown) {  // shift to zoom
      // TODO shift to zoom on cursor location
      val increment = -event.getDeltaX  // shifts X/Y axes: https://stackoverflow.com/questions/42429591/javafx-shiftscrollwheel-always-return-0-0
      val range = lastAxis.getUpperBound - lastAxis.getLowerBound
      val zoomFactor = Math.pow(1.01, increment)
      val mid = (lastAxis.getLowerBound + lastAxis.getUpperBound) / 2
      (mid - (range * zoomFactor / 2), mid + (range * zoomFactor / 2))
    } else {  // normal scroll, left/right
      val increment = -event.getDeltaY
      val range = lastAxis.getUpperBound - lastAxis.getLowerBound
      val shift = (range / 256) * increment
      (lastAxis.getLowerBound + shift, lastAxis.getUpperBound + shift)
    }

    charts.foreach(chart => {
      chart.timeAxis.setLowerBound(newLower)
      chart.timeAxis.setUpperBound(newUpper)
      ChartUpdator.updateChart(chart.chart, chart.data)
    })
  }

  protected def onMouse(event: MouseEvent): Unit = {

  }

  def zoomMax(): Unit = {
    val minTime = charts.map(_.data.minTime).min
    val maxTime = charts.map(_.data.maxTime).max

    println(s"Zoom max: $minTime -> $maxTime")

    charts.foreach(chart => {
      chart.timeAxis.setLowerBound(minTime)
      chart.timeAxis.setUpperBound(maxTime)
      ChartUpdator.updateChart(chart.chart, chart.data)
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
  val visualizationPane = new StackPane(delegate=new BTreeChart(batteriesTree, 1000))

//  val visualizationPane = new SharedAxisCharts
//  visualizationPane.addChart(lineChart1, batteriesTree)
//  visualizationPane.addChart(lineChart2)

//  visualizationPane.zoomMax()

  stage = new PrimaryStage {
    title = "Big Data Visualizer"
    scene = new Scene {
      val splitPane = new SplitPane {
        items ++= Seq(navigationPane, visualizationPane)
        SplitPane.setResizableWithParent(navigationPane, false)
        SplitPane.setResizableWithParent(visualizationPane, false)
      }
      splitPane.setDividerPositions(0.25)
      root = splitPane
    }
  }
}
