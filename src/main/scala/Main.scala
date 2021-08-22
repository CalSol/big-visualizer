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
    val timeAxis = new NumberAxis()
    timeAxis.setAutoRanging(false)

    val rawData1 = (0 until 1024).map { i => (i, i + 64 * Math.random()) }
    val data1 = ObservableBuffer(rawData1 map {case (x, y) => XYChart.Data[Number, Number](x, y)})
    val series1 = XYChart.Series[Number, Number]("test1", data1)
    val data1Axis = new NumberAxis()
//    data1Axis.setAutoRanging(false)
    val lineChart1 = LineChart(timeAxis, data1Axis)
    lineChart1.title = "TestChart1"
    lineChart1.getData.add(series1)

    val rawData2 = (0 until 1024).map { i => (i, -i - 64 * Math.random()) }
    val data2 = ObservableBuffer(rawData2 map {case (x, y) => XYChart.Data[Number, Number](x, y)})
    val series2 = XYChart.Series[Number, Number]("test2", data2)
    val data2Axis = new NumberAxis()
//    data2Axis.setAutoRanging(false)
    val lineChart2 = LineChart(timeAxis, data2Axis)
    lineChart2.title = "TestChart2"
    lineChart2.getData.add(series2)

    children = Seq(lineChart1, lineChart2)
    setVgrow(lineChart1, Priority.Always)
    setVgrow(lineChart2, Priority.Always)

    val SCALE_DELTA = 1.1
    lineChart2.setOnScroll(new EventHandler[ScrollEvent] {
      override def handle(event: ScrollEvent): Unit = {
        println(s"scroll ${event.getDeltaX} ${event.getDeltaY}")
        event.consume()
        if (event.getDeltaY == 0) {
          return
        }
        val scaleFactor = if (event.getDeltaY > 0) SCALE_DELTA else 1 / SCALE_DELTA
        timeAxis.setLowerBound(timeAxis.getLowerBound + event.getDeltaY)
        timeAxis.setUpperBound(timeAxis.getUpperBound + event.getDeltaY)
        println(s"${timeAxis.getLowerBound}, ${timeAxis.getUpperBound}")
      }
    })

    lineChart2.setOnMousePressed(new EventHandler[MouseEvent]() {
      override def handle(event: MouseEvent): Unit = {
        println(s"press ${event.getClickCount}")

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
