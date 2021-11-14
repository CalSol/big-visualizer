package bigvis.control
import bigvis.{BTreeDataItem, CsvLoader}
import javafx.scene.input.{DragEvent, MouseEvent}
import scalafx.scene.control.{TreeItem, TreeTableColumn, TreeTableRow, TreeTableView}
import scalafx.scene.input.{ClipboardContent, TransferMode}

import scala.jdk.CollectionConverters.ListHasAsScala

class DataTreeView(root: TreeItem[BTreeDataItem]) extends TreeTableView[BTreeDataItem](root) {
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

  this.setOnDragOver((event: DragEvent) => {
    event.acceptTransferModes(TransferMode.Copy)
    event.consume()
  })

  this.setOnDragDropped((event: DragEvent) => {
    event.getDragboard.getFiles.asScala.toSeq match {
      case Seq(file) if file.getName.endsWith(".csv") =>
        val statusTreeItem = new TreeItem(BTreeDataItem(file.getName, "loading", None))
        this.root.children.append(statusTreeItem)

        new Thread(() => {  // read in a separate thread, so the UI loop doesn't freeze
          try {
            val loadedDataItems = CsvLoader.load(file.toPath) { status =>
              statusTreeItem.value.value.dataProp.value = status
            }
            System.gc() // this saves ~1-2 GB of memory

            val loadedTreeItems = loadedDataItems.map(new TreeItem(_).delegate)
            this.root.children.appendAll(loadedTreeItems)
          } catch {
            case e: Exception =>
              statusTreeItem.value.value.dataProp.value = e.toString
              Thread.sleep(5000)
          } finally {
            this.root.children.remove(statusTreeItem)
          }
        }).start()
        event.setDropCompleted(true)
      case _ =>
        event.setDropCompleted(false)
    }
    event.consume()
  })

  this.setRowFactory((p: javafx.scene.control.TreeTableView[BTreeDataItem]) => {
    val row = new TreeTableRow[BTreeDataItem]()
    row.setOnDragDetected((event: MouseEvent) => {
      val draggedItem = row.treeItem.getValue.getValue
      draggedItem.tree match {
        case Some(itemTree) =>
          val dragBoard = row.startDragAndDrop(TransferMode.Copy)
          val content = new ClipboardContent()
          content.putString(draggedItem.name)
          dragBoard.setContent(content)
        case _ =>
      }
      event.consume()
    })
    row
  })
}
