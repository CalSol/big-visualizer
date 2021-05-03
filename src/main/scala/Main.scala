package bigvis

import scalafx.application.JFXApp
import scalafx.application.JFXApp.PrimaryStage
import scalafx.beans.property.StringProperty
import scalafx.collections.ObservableBuffer
import scalafx.scene.Scene
import scalafx.scene.chart.{LineChart, NumberAxis, XYChart}
import scalafx.scene.control.{SplitPane, TreeItem, TreeTableColumn, TreeTableView}
import scalafx.scene.layout.VBox


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
  }

  val visualizationPane = new VBox {
    val timeAxis = new NumberAxis()
    val dataAxis = new NumberAxis()
    val lineChart = LineChart(timeAxis, dataAxis)
    lineChart.title = "TestChart"

    val data = ObservableBuffer(Seq(
      (1, 23),
      (2, 14),
      (3, 15),
      (4, 24),
      (5, 34),
      (6, 36),
      (7, 22),
      (8, 45),
      (9, 43),
      (10, 17),
      (11, 29),
      (12, 25)
    ) map {case (x, y) => XYChart.Data[Number, Number](x, y)})
    val series = XYChart.Series[Number, Number]("test", data)
    lineChart.getData.add(series)

    children = Seq(lineChart)
  }

  stage = new PrimaryStage {
    title = "Big Data Visualizer"
    scene = new Scene {
      content = new SplitPane {
        items ++= Seq(navigationPane, visualizationPane)
      }
    }
  }
}
