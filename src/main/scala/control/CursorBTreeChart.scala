package bigvis
package control

import btree.BTree

import scalafx.scene.paint.Color


// Mixin for B-tree charts that provides a live (follows user's mouse) cursor with data dispaly
// IMPORTANT: the cursor canvas is not added by default, since it should be the last item added
// (on top of everything else) - this must be done in the implementing class.
trait CursorBTreeChart { this: BaseBTreeChart =>
  // implement this to set the values to be displayed under the cursor
  def getCursorData(scale: ChartParameters, xPos: BTree.TimestampType): Seq[(String, Double, Color)]


  val cursorCanvas = new CursorCanvas()
  cursorCanvas.widthProperty().bind(width)
  cursorCanvas.heightProperty().bind(height)
  Seq(container.cursorXPos).foreach { observable =>
    observable.onInvalidate(
      redrawCursor()
    )
  }

  protected def redrawCursor(): Unit = {
    val scale = ChartParameters(width.value.toInt, height.value.toInt,
      container.xAxis.value, yAxis.value, container.timeZone)
    val cursorPos = container.cursorXPos.value
    val cursorTime = scale.xPosToVal(cursorPos)
    cursorCanvas.draw(scale, cursorPos, getCursorData(scale, cursorTime))
  }
}
