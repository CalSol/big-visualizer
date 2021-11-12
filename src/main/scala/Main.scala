package bigvis

import btree.BTree.TimestampType
import btree.{BTree, FloatAggregator}

import com.github.tototoshi.csv.CSVReader
import javafx.scene.input.{MouseEvent, ScrollEvent}
import scalafx.application.JFXApp
import scalafx.application.JFXApp.PrimaryStage
import scalafx.beans.property.StringProperty
import scalafx.scene.Scene
import scalafx.scene.control.{SplitPane, TreeItem, TreeTableColumn, TreeTableView}
import scalafx.scene.layout.VBox.setVgrow
import scalafx.scene.layout.{Priority, StackPane, VBox}

import java.io.File
import scala.collection.mutable


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

    val (newYLower, newYUpper) = if (event.isShiftDown && event.isControlDown){ //vertical scroll
      val increment = -event.getDeltaX  // shifts X/Y axes: https://stackoverflow.com/questions/42429591/javafx-shiftscrollwheel-always-return-0-0
      val range = lastChart.yUpper.value - lastChart.yLower.value
      val mouseFrac = 1 - event.getY / lastChart.getHeight
      val mouseValue = lastChart.yLower.value + (range * mouseFrac)
      val newRange = range * Math.pow(1.01, increment)
      (mouseValue - (newRange * mouseFrac), mouseValue + (newRange * (1 - mouseFrac)))
    } else if (event.isShiftDown) {
      val increment = -event.getDeltaX
      val range = lastChart.yUpper.value - lastChart.yLower.value
      val shift = (range / 256) * increment
      (lastChart.yLower.value + shift, lastChart.yUpper.value + shift)
    } else {
        (lastChart.yLower.value,lastChart.yUpper.value)
    }

    val (newXLower, newXUpper) = if (event.isControlDown) {  // shift to zoom
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
      if (newXLower != chart.chart.xLower.value) {
        chart.chart.xLower.value = newXLower
        chart.chart.xUpper.value = newXUpper
      }
      if (newYLower != chart.chart.yLower.value) {
        chart.chart.yLower.value = newYLower
        chart.chart.yUpper.value = newYUpper
      }
    })
  }

  protected def onMouse(event: MouseEvent): Unit = {
    charts.foreach(chart => {
      chart.chart.cursorXPos.value = event.getX
    })
  }

  def zoomMax(): Unit = {
    val minXTime = charts.map(_.chart.xLower.value).min
    val maxXTime = charts.map(_.chart.xUpper.value).max
    val minYTime = charts.map(_.chart.yLower.value).min
    val maxYTime = charts.map(_.chart.yUpper.value).max
    charts.foreach(chart => {
        chart.chart.xLower.value = minXTime
        chart.chart.xUpper.value = maxXTime
        chart.chart.yLower.value = minYTime
        chart.chart.yUpper.value = maxYTime
    })
  }
}


object Main extends JFXApp {
  // See layouts documentation
  // https://docs.oracle.com/javafx/2/layout/builtin_layouts.htm

  println(s"Rendering pipeline: ${com.sun.prism.GraphicsPipeline.getPipeline.getClass.getName}")

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
  // TODO open all 28
  val cellTrees = (0 until 4).map{ _ => new BTree(FloatAggregator.aggregator, 16) }

  {
    val cellArrs = cellTrees.map{ _ => new mutable.ArrayBuffer[(TimestampType, Float)]() }

//    val reader = CSVReader.open(new File("../big-analysis/Fsgp21Decode/bms.pack.voltage.csv"))
    val reader = CSVReader.open(new File("bms.cell.voltage.csv"))
    println("Map data")
    val batteriesIterator = reader.iterator
    batteriesIterator.next()  // skip header row
    batteriesIterator.foreach { fields => // tail to discard header
       fields(0).toDoubleOption match {
         case Some(time) =>
           val timeLong = (time * 1000).toLong
           cellArrs.zipWithIndex.map { case (cellArr, i) =>
             fields(2 + i).toFloatOption match {
               case Some(field) => cellArr.append((timeLong, field))
               case None =>
             }
           }
         case None =>
      }
    }
    println(f"data read, n=${cellArrs(0).length}")

    (cellTrees zip cellArrs).zipWithIndex.foreach { case ((cellTree, cellArr), i) =>
      cellTree.appendAll(cellArr)
      println(s"tree insert $i, h=${cellTree.maxDepth}")
    }
  }
  System.gc()  // this saves ~1-2 GB of memory

  // TODO make this much less hacky =s
  val chartDefs = (cellTrees zip ChartTools.createColors(cellTrees.length)).zipWithIndex.map { case ((cellTree, cellColor), i) =>
    ChartDefinition(f"cell-$i", cellTree, cellColor)
  }

  // TODO the wrapping doesn't belong here
  val visualizationPane = new SharedAxisCharts
  visualizationPane.addChart(new StackPane(delegate=
    new BTreeChart(chartDefs, 1000)))

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
