package bigvis

import control.{DataTreeView, SharedAxisCharts}

import scalafx.application.JFXApp
import scalafx.application.JFXApp.PrimaryStage
import scalafx.scene.Scene
import scalafx.scene.control.{SplitPane, TreeItem}
import scalafx.scene.layout.VBox.setVgrow
import scalafx.scene.layout.{Priority, VBox}


object Main extends JFXApp {
  println(s"Rendering pipeline: ${com.sun.prism.GraphicsPipeline.getPipeline.getClass.getName}")

  // See layouts documentation
  // https://docs.oracle.com/javafx/2/layout/builtin_layouts.htm
  val dataRoot = new TreeItem(BTreeDataItem("root", "", None)) {
    expanded = true
    children = Seq()
  }

  val navigationPane = new VBox {
    // See table view example at
    // https://github.com/scalafx/ScalaFX-Tutorials/blob/master/slick-table/src/main/scala/org/scalafx/slick_table/ContactsView.scala
    // and tree view example at
    // https://github.com/scalafx/scalafx/blob/master/scalafx-demos/src/main/scala/scalafx/controls/treetableview/TreeTableViewWithTwoColumns.scala
    val tree = new DataTreeView(dataRoot)
    children = Seq(tree)
    setVgrow(tree, Priority.Always)
  }


  // TODO make this much less hacky =s
//  val chartDefs = (cellTrees zip ChartTools.createColors(cellTrees.length)).zipWithIndex.map { case ((cellTree, cellColor), i) =>
//    ChartDefinition(f"cell-$i", cellTree, cellColor)
//  }
//  val chartDefs = Seq()

  // TODO the wrapping doesn't belong here
  val visualizationPane = new SharedAxisCharts
//  visualizationPane.addChart(new StackPane(delegate=
//    new BTreeChart(chartDefs, 1000)))

//  visualizationPane.zoomMax()

  stage = new PrimaryStage {
    title = "Big Data Visualizer"
    width = 660
    height = 550
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
