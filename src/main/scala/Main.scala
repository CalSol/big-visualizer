package bigvis

import control.{DataTreeView, PerfTreeView, SharedAxisCharts}

import javafx.event.{ActionEvent, EventHandler}
import scalafx.animation.{KeyFrame, Timeline}
import scalafx.application.JFXApp
import scalafx.application.JFXApp.PrimaryStage
import scalafx.scene.Scene
import scalafx.scene.control.{Label, SplitPane}
import scalafx.scene.layout.VBox.setVgrow
import scalafx.scene.layout.{HBox, Priority, VBox}
import scalafx.stage.Stage
import scalafx.util.Duration


object Main extends JFXApp {
  val perfTree = new PerfTreeView()
  val memoryLabel = new Label(s"(memory)")
  val perfStage = new Stage() {
    title = "Performance"
    scene = new Scene {
      root = new VBox {
        children = Seq(
          new HBox {
            children = Seq(
              new Label(s"Rendering pipeline: ${com.sun.prism.GraphicsPipeline.getPipeline.getClass.getName}"),
              memoryLabel
            )
          },
          perfTree
        )
      }
    }
  }
  perfStage.show()

  val runtime = Runtime.getRuntime
  val memoryTimer = Timeline(Seq(
    KeyFrame(Duration(250), onFinished = (t: ActionEvent) => {
      memoryLabel.text = f"${(runtime.totalMemory() - runtime.freeMemory()) / 1024 / 1024} MiB"
    })
  ))
  memoryTimer.setCycleCount(Timeline.Indefinite)
  memoryTimer.play()


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
    width = 1280
    height = 768
    scene = new Scene {
      val splitPane = new SplitPane {
        items ++= Seq(navigationPane, visualizationPane)
        SplitPane.setResizableWithParent(navigationPane, false)
      }
      splitPane.setDividerPositions(0.25)
      root = splitPane
    }
    onCloseRequest = { _ =>
      perfStage.close()  // also close the performance window when the main window closes
    }
  }
}
