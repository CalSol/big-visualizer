package bigvis

import bigvis.btree.{BTree, FloatAggregate}
import javafx.scene.input.{MouseEvent, ScrollEvent}
import scalafx.application.JFXApp
import scalafx.application.JFXApp.PrimaryStage
import scalafx.beans.property.StringProperty
import scalafx.collections.ObservableBuffer
import scalafx.scene.Scene
import scalafx.scene.chart.{LineChart, NumberAxis, XYChart}
import scalafx.scene.control.{SplitPane, TreeItem, TreeTableColumn, TreeTableView}
import scalafx.scene.layout.{Priority, VBox}
import scalafx.scene.layout.VBox.setVgrow

import java.io.File
import collection.mutable
import com.github.tototoshi.csv.CSVReader

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

    val nodes = data.getNodes(lower.toLong, upper.toLong, (range / widthPixels).toLong)
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
    charts.foreach(chart => {
      chart.timeAxis.setLowerBound(lastAxis.getLowerBound - event.getDeltaY)
      chart.timeAxis.setUpperBound(lastAxis.getUpperBound - event.getDeltaY)
      ChartUpdator.updateChart(chart.chart, chart.data)
    })
  }

  protected def onMouse(event: MouseEvent): Unit = {

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
  val batteriesTree = new BTree(FloatAggregate.aggregator, 32)
  val reader = CSVReader.open(new File("../big-analysis/Fsgp21Decode/bms.pack.voltage.csv"))
  println("Map data")
  val batteriesData = reader.toStream.tail.flatMap { fields =>  // tail to discard header
    (fields(0).toFloatOption, fields(2).toFloatOption) match {
      case (Some(time), Some(data)) => Some(((time * 1000).toLong, data))
      case _ => None
    }
  }
  println(s"batteries data: ${batteriesData.length}")

  batteriesTree.appendAll(batteriesData)
  println(s"tree insert, h=${batteriesTree.maxDepth}")


  val rawData1 = (0 until 1024).map { i => (i, i + 64 * Math.random()) }
  val data1 = ObservableBuffer(rawData1 map {case (x, y) => XYChart.Data[Number, Number](x, y)})
//  println("Map fields to chart")
//  val data1 = ObservableBuffer(readData map {case (x, y) => XYChart.Data[Number, Number](x, y)})
  println("Create chart")
  val series1 = XYChart.Series[Number, Number]("test1", data1)
  val lineChart1 = LineChart(new NumberAxis(), new NumberAxis())
  println("Add data")
  lineChart1.getData.add(series1)
  lineChart1.title = "TestChart1"
  println("done")

  val rawData2 = (0 until 1024).map { i => (i, -i + 64 * Math.random()) }
  val data2 = ObservableBuffer(rawData2 map {case (x, y) => XYChart.Data[Number, Number](x, y)})
  val series2 = XYChart.Series[Number, Number]("test2", data2)
  val lineChart2 = LineChart(new NumberAxis(), new NumberAxis())
  lineChart2.getData.add(series2)
  lineChart2.title = "TestChart2"

  val visualizationPane = new SharedAxisCharts
  visualizationPane.addChart(lineChart1, batteriesTree)
//  visualizationPane.addChart(lineChart2)

  stage = new PrimaryStage {
    title = "Big Data Visualizer"
    scene = new Scene {
      val splitPane = new SplitPane {
        items ++= Seq(navigationPane, visualizationPane)
        SplitPane.setResizableWithParent(navigationPane, false)
      }
      splitPane.setDividerPositions(0.25)
      root = splitPane
    }
  }
}
