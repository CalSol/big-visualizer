package bigvis
package control

import javafx.event.{Event, EventDispatchChain}
import javafx.scene.paint.Color
import scalafx.Includes._
import scalafx.scene.input.{DragEvent, TransferMode}
import scalafx.scene.layout.StackPane
import scalafx.scene.shape.Rectangle


class DraggableBTreeChartWrapper(chart: BaseBTreeChart) extends StackPane {
  protected val dragRect = Rectangle(0, 20, width.value, height.value - 40)
  dragRect.setFill(Color.TRANSPARENT)
  Seq(width, height).foreach { observable =>
    observable.onChange{
      dragRect.setWidth(width.value)
      dragRect.setHeight(height.value - 40)
    }
  }

  dragRect.onDragOver = (event: DragEvent) => {
    event.dragboard.content.get(DataTreeView.BTreeDataType) match {
      case Some(str: String) =>
        event.acceptTransferModes(TransferMode.Copy)
        dragRect.setFill(Color.BLUE.deriveColor(0, 1, 1, 0.25))
      case _ =>
        dragRect.setFill(Color.RED.deriveColor(0, 1, 1, 0.25))
    }
    event.consume()
  }
  dragRect.onDragExited = (event: DragEvent) => {
    dragRect.setFill(Color.TRANSPARENT)
    event.consume()
  }
  dragRect.onDragDropped = (event: DragEvent) => {
    event.dragboard.content.get(DataTreeView.BTreeDataType) match {
      case Some(str: String) => chart.container.dataItems.get(str).foreach { bTreeData =>
        PerfTreeView().foreach(_.addItem(bTreeData.name))
        chart.addDataset(bTreeData)
        dragRect.setFill(Color.TRANSPARENT)
      }
      case _ => // shouldn't get here
    }
    event.consume()
  }

  // Delegate non-consumed events to the chart, since the drag rent only handles the events above
  val oldDragEventHandler = dragRect.eventDispatcher.value
  dragRect.setEventDispatcher((event: Event, eventDispatchChain: EventDispatchChain) => {
    val returnedEvent = oldDragEventHandler.dispatchEvent(event, eventDispatchChain)
    if (returnedEvent != null && !returnedEvent.isConsumed) {
      chart.eventDispatcher.value.dispatchEvent(returnedEvent, eventDispatchChain)
    } else {
      returnedEvent
    }
  })

  val chartsPane = new StackPane {
    minWidth = 0
    minHeight = 0
    children = Seq(chart, dragRect)
  }
  children.append(chartsPane)

}
