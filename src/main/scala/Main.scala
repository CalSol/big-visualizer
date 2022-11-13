package bigvis

import control.{DataTreeView, PerfTreeView, SharedAxisCharts}

import scalafx.application.JFXApp
import scalafx.application.JFXApp.PrimaryStage
import scalafx.scene.Scene
import scalafx.scene.control.{Label, SplitPane}
import scalafx.scene.layout.VBox.setVgrow
import scalafx.scene.layout.{GridPane, Priority, VBox}
import scalafx.stage.Stage


object Main extends JFXApp {
  val perfTree = new PerfTreeView()
  val perfStage = new Stage() {
    title = "Performance"
    scene = new Scene {
      root = new GridPane() {
        add(new Label(s"Rendering pipeline: ${com.sun.prism.GraphicsPipeline.getPipeline.getClass.getName}"), 0, 0)
        add(perfTree, 0, 1)
      }
    }
  }
  perfStage.show()


  // See layouts documentation
  // https://docs.oracle.com/javafx/2/layout/builtin_layouts.htm
  val navigationPane = new VBox {
    // See table view example at
    // https://github.com/scalafx/ScalaFX-Tutorials/blob/master/slick-table/src/main/scala/org/scalafx/slick_table/ContactsView.scala
    // and tree view example at
    // https://github.com/scalafx/scalafx/blob/master/scalafx-demos/src/main/scala/scalafx/controls/treetableview/TreeTableViewWithTwoColumns.scala
    val tree = new DataTreeView()
    children = Seq(tree)
    setVgrow(tree, Priority.Always)
  }

  val visualizationPane = new SharedAxisCharts(navigationPane.tree.dataItems)

  stage = new PrimaryStage {
    title = "Big Data Visualizer"
    width = 660
    height = 550
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
