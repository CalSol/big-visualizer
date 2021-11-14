package bigvis.control
import bigvis.{BTreeData, CsvLoader}
import javafx.scene.input.{DragEvent, MouseEvent}
import scalafx.beans.property.StringProperty
import scalafx.scene.control.{TreeItem, TreeTableColumn, TreeTableRow, TreeTableView}
import scalafx.scene.input.{ClipboardContent, DataFormat, TransferMode}

import scala.jdk.CollectionConverters.{ListHasAsScala, SeqHasAsJava}


object DataTreeView {
  val BTreeDataType = new DataFormat ("application/x-java-serialized-object")
}


class DataTreeItem(name: String, desc: String, val payload: Any = None) {
  val nameProp = StringProperty(name)
  val dataProp = StringProperty(desc)
}


class DataTreeView extends TreeTableView[DataTreeItem]() {
  this.setRoot(new TreeItem(new DataTreeItem("root", "", None)) {
    expanded = true
    children = Seq()
  })

  columns ++= Seq(
    new TreeTableColumn[DataTreeItem, String] {
      text = "Name"
      cellValueFactory = { _.value.value.value.nameProp }
    },
    new TreeTableColumn[DataTreeItem, String] {
      text = "Data"
      cellValueFactory = { _.value.value.value.dataProp }
    }
  )

  this.setOnDragOver((event: DragEvent) => {
    event.acceptTransferModes(TransferMode.Copy)
    event.consume()
  })

  this.setOnDragDropped((event: DragEvent) => {
    event.getDragboard.getFiles.asScala.toSeq match {
      case Seq(file) if file.getName.endsWith(".csv") =>
        val statusTreeItem = new TreeItem(new DataTreeItem(file.getName, "loading", None))
        this.root.value.getChildren.add(statusTreeItem)

        new Thread(() => {  // read in a separate thread, so the UI loop doesn't freeze
          try {
            val loadedData = CsvLoader.load(file.toPath) { status =>
              statusTreeItem.value.value.dataProp.value = status
            }
            System.gc() // this saves ~1-2 GB of memory

            val loadedTreeItems = loadedData.map { loaded =>
              new TreeItem(new DataTreeItem(
                loaded.name,
                s"${loaded.tree.length} (${loaded.tree.aggregatorType.getClass.getName.split('.').last})",
                loaded
              )).delegate
            }
            this.root.value.getChildren.addAll(loadedTreeItems.asJava)
          } catch {
            case e: Exception =>
              statusTreeItem.value.value.dataProp.value = e.toString
              Thread.sleep(5000)
          } finally {
            this.root.value.getChildren.remove(statusTreeItem)
          }
        }).start()
        event.setDropCompleted(true)
      case _ =>
        event.setDropCompleted(false)
    }
    event.consume()
  })

  this.setRowFactory((p: javafx.scene.control.TreeTableView[DataTreeItem]) => {
    val row = new TreeTableRow[DataTreeItem]()
    row.setOnDragDetected((event: MouseEvent) => {
      val draggedItem = row.treeItem.getValue.getValue
      draggedItem.payload match {
        case bTreeData: BTreeData =>
          val dragBoard = row.startDragAndDrop(TransferMode.Copy)
          val content = new ClipboardContent()
          content.put(DataTreeView.BTreeDataType, bTreeData.name)
          dragBoard.setContent(content)
        case _ =>
      }
      event.consume()
    })
    row
  })
}
