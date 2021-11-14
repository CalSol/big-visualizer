package bigvis

import btree.BTree.TimestampType
import btree.{BTree, BTreeAggregator, FloatAggregator}

import com.github.tototoshi.csv.CSVReader
import javafx.scene.input.{DragEvent, MouseEvent, ScrollEvent}
import scalafx.application.JFXApp
import scalafx.application.JFXApp.PrimaryStage
import scalafx.beans.property.StringProperty
import scalafx.scene.Scene
import scalafx.scene.control.{SplitPane, TreeItem, TreeTableColumn, TreeTableView}
import scalafx.scene.input.TransferMode
import scalafx.scene.layout.VBox.setVgrow
import scalafx.scene.layout.{Priority, StackPane, VBox}

import java.io.File
import java.net.URI
import java.nio.file.{FileSystem, LinkOption, Path, Paths, WatchEvent, WatchKey, WatchService}
import scala.collection.mutable
import scala.jdk.CollectionConverters.CollectionHasAsScala


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

    if (event.isShiftDown) {  // TODO implement vertical zoom/pan
      val increment = -event.getDeltaX  // shifts X/Y axes: https://stackoverflow.com/questions/42429591/javafx-shiftscrollwheel-always-return-0-0

    } else {

    }

    val (newLower, newUpper) = if (event.isControlDown) {  // shift to zoom
      val increment = -event.getDeltaY  // consistent with Chrome's zoom UI

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
    charts.foreach(chart => {
      chart.chart.cursorXPos.value = event.getX
    })
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
    val tree = new TreeTableView[BTreeDataItem](dataRoot) {
      columns ++= Seq(
        new TreeTableColumn[BTreeDataItem, String] {
          text = "Name"
          cellValueFactory = { _.value.value.value.nameProp }
        },
        new TreeTableColumn[BTreeDataItem, String] {
          text = "Data"
          cellValueFactory = { _.value.value.value.dataProp }
        }
      )
    }
    children = Seq(tree)
    setVgrow(tree, Priority.Always)
  }

  navigationPane.tree.setOnDragOver((event: DragEvent) => {
    event.acceptTransferModes(TransferMode.Copy)
    event.consume()
  })
  navigationPane.tree.setOnDragDropped((event: DragEvent) => {
    event.getDragboard.getFiles.asScala.toSeq match {
      case Seq(file) if file.getName.endsWith(".csv") =>
        val statusTreeItem = new TreeItem(BTreeDataItem(file.getName, "loading", None))
        dataRoot.children.append(statusTreeItem)

        new Thread(() => {  // read in a separate thread, so the UI loop doesn't freeze
          try {
            val loadedDataItems = CsvLoader.load(file.toPath) { status =>
              statusTreeItem.value.value.dataProp.value = status
            }
            System.gc()  // this saves ~1-2 GB of memory

            val loadedTreeItems = loadedDataItems.map(new TreeItem(_).delegate)
            dataRoot.children.appendAll(loadedTreeItems)
          } finally {
            dataRoot.children.remove(statusTreeItem)
          }
        }).start()
        event.setDropCompleted(true)
      case _ =>
        event.setDropCompleted(false)
    }
    event.consume()
  })

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
