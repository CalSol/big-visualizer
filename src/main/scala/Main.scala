package bigvis

import javafx.event.EventHandler
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


// TODO split into data model
case class DataItem(name: String) {
  val nameProp = StringProperty(name)
  val dataProp = StringProperty("0")
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

  val visualizationPane = new VBox {
    val rawData1 = (0 until 1024).map { i => (i, i + 64 * Math.random()) }
    val data1 = ObservableBuffer(rawData1 map {case (x, y) => XYChart.Data[Number, Number](x, y)})
    val series1 = XYChart.Series[Number, Number]("test1", data1)
    val time1Axis = new NumberAxis()
    time1Axis.setAnimated(false)
    time1Axis.setAutoRanging(false)
    val data1Axis = new NumberAxis()
//    data1Axis.setAutoRanging(false)
    val lineChart1 = LineChart(time1Axis, data1Axis)
    lineChart1.title = "TestChart1"
    lineChart1.getData.add(series1)
    lineChart1.getYAxis.setTickLabelRotation(270)  // to keep the chart size the same so stacked charts are consistent
    lineChart1.setLegendVisible(false)
    lineChart1.getXAxis.setVisible(false)  // only last chart needs visible time axis
    lineChart1.getXAxis.setTickMarkVisible(false)
    lineChart1.getXAxis.setTickLabelsVisible(false)

    val rawData2 = (0 until 1024).map { i => (i, -i - 64 * Math.random()) }
    val data2 = ObservableBuffer(rawData2 map {case (x, y) => XYChart.Data[Number, Number](x, y)})
    val series2 = XYChart.Series[Number, Number]("test2", data2)
    val time2Axis = new NumberAxis()
    time2Axis.setAnimated(false)
    time2Axis.setAutoRanging(false)
    val data2Axis = new NumberAxis()
//    data2Axis.setAutoRanging(false)
    val lineChart2 = LineChart(time2Axis, data2Axis)
    lineChart2.title = "TestChart2"
    lineChart2.getData.add(series2)
    lineChart2.getYAxis.setTickLabelRotation(270)  // to keep the chart size the same so stacked charts are consistent
    lineChart2.setLegendVisible(false)

    children = Seq(lineChart1, lineChart2)
    setVgrow(lineChart1, Priority.Always)
    setVgrow(lineChart2, Priority.Always)

    val SCALE_DELTA = 1.1
    lineChart2.setOnScroll(new EventHandler[ScrollEvent] {
      override def handle(event: ScrollEvent): Unit = {
        event.consume()
        if (event.getDeltaY == 0) {
          return
        }
        val scaleFactor = if (event.getDeltaY > 0) SCALE_DELTA else 1 / SCALE_DELTA
        time1Axis.setLowerBound(time1Axis.getLowerBound + event.getDeltaY)
        time1Axis.setUpperBound(time1Axis.getUpperBound + event.getDeltaY)
        time2Axis.setLowerBound(time2Axis.getLowerBound + event.getDeltaY)
        time2Axis.setUpperBound(time2Axis.getUpperBound + event.getDeltaY)
      }
    })

    lineChart2.setOnMousePressed(new EventHandler[MouseEvent]() {
      override def handle(event: MouseEvent): Unit = {
        if (event.getClickCount == 2) {
          lineChart2.setScaleX(1.0)
          lineChart2.setScaleY(1.0)
        }
      }
    })
  }

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
