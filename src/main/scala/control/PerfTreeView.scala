package bigvis
package control

import scalafx.Includes._
import scalafx.beans.property.StringProperty
import scalafx.scene.control.{TreeItem, TreeTableColumn, TreeTableView}

import scala.collection.mutable


// Singleton
object PerfTreeView {
  private var instance: Option[PerfTreeView] = None

  def apply(): Option[PerfTreeView] = instance
}


class PerfTreeItem(name: String) {
  val nameProp = StringProperty(name)
  val nodeCountProp = StringProperty("")
  val resampleNodeCountProp = StringProperty("")
  val nodeTimeProp = StringProperty("")
  val resampleTimeProp = StringProperty("")
  val sectionTimeProp = StringProperty("")
  val renderTimeProp = StringProperty("")
}


class PerfTreeView extends TreeTableView[PerfTreeItem]() {
  this.setRoot(new TreeItem(new PerfTreeItem("root")) {
    expanded = true
    children = Seq()
  })
  this.setShowRoot(false)

  // note: multiple instances of a data series not supported
  val items = mutable.HashMap[String, PerfTreeItem]()

  def addItem(name: String): Unit = {
    if (items.contains(name)) {  // duplicates ignored, it'll resolve to the prior one
      return
    }
    val newItem = new PerfTreeItem(name)
    this.getRoot.getChildren.append(new TreeItem(newItem))
    items.put(name, newItem)
  }

  // Update the data processing related performance stats for an item
  def updateItemPerf(name: String, nodeCount: Long, resampleNodeCount: Long,
                     nodeTime: Double, sectionTime: Double, resampleTime: Double): Unit = {
    items.get(name).foreach { item =>
      item.nodeCountProp.setValue(nodeCount.toString)
      item.resampleNodeCountProp.setValue(resampleNodeCount.toString)
      item.nodeTimeProp.setValue(f"${nodeTime * 1000}%.1f ms")
      item.sectionTimeProp.setValue(f"${sectionTime * 1000}%.1f ms")
      item.resampleTimeProp.setValue(f"${resampleTime * 1000}%.1f ms")
    }
  }

  // Update the rendering performance stats for an item
  def updateItemRender(name: String, renderTime: Double): Unit = {
    items.get(name).foreach { item =>
      item.renderTimeProp.setValue(f"${renderTime * 1000}%.1f ms")
    }
  }

  columns ++= Seq(
    new TreeTableColumn[PerfTreeItem, String] {
      text = "Name"
      cellValueFactory = { _.value.value.value.nameProp }
    },
    new TreeTableColumn[PerfTreeItem, String] {
      text = "Nodes"
      cellValueFactory = { _.value.value.value.nodeCountProp }
    },
    new TreeTableColumn[PerfTreeItem, String] {
      text = "Resampled Nodes"
      cellValueFactory = { _.value.value.value.resampleNodeCountProp }
    },
    new TreeTableColumn[PerfTreeItem, String] {
      text = "Node Time"
      cellValueFactory = { _.value.value.value.nodeTimeProp }
    },
    new TreeTableColumn[PerfTreeItem, String] {
      text = "Resample Time"
      cellValueFactory = { _.value.value.value.resampleTimeProp }
    },
    new TreeTableColumn[PerfTreeItem, String] {
      text = "Section Time"
      cellValueFactory = { _.value.value.value.sectionTimeProp }
    },
    new TreeTableColumn[PerfTreeItem, String] {
      text = "Resample Time"
      cellValueFactory = { _.value.value.value.resampleTimeProp }
    },
    new TreeTableColumn[PerfTreeItem, String] {
      text = "Render Time"
      cellValueFactory = { _.value.value.value.renderTimeProp }
    },
  )

  require(PerfTreeView.instance.isEmpty, "PerfTreeView must be singleton")
  PerfTreeView.instance = Some(this) // at this point it's mutable
}
