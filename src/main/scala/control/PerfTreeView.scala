package bigvis
package control

import scalafx.beans.property.StringProperty
import scalafx.scene.control.{TreeItem, TreeTableColumn, TreeTableView}


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
  val sectionTimeProp = StringProperty("")
  val resampleTimeProp = StringProperty("")
}


class PerfTreeView extends TreeTableView[PerfTreeItem]() {
  this.setRoot(new TreeItem(new PerfTreeItem("root")) {
    expanded = true
    children = Seq()
  })
  this.setShowRoot(false)

  require(PerfTreeView.instance.isEmpty, "PerfTreeView must be singleton")
  PerfTreeView.instance = Some(this)  // at this point it's mutable

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
      text = "Section Time"
      cellValueFactory = { _.value.value.value.sectionTimeProp }
    },
    new TreeTableColumn[PerfTreeItem, String] {
      text = "Resample Time"
      cellValueFactory = { _.value.value.value.resampleTimeProp }
    },
  )
}
