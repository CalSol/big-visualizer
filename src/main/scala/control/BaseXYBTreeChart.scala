package bigvis
package control

import scalafx.Includes._
import scalafx.beans.property.DoubleProperty
import scalafx.scene.input.ScrollEvent


// Base XY BTreeChart that also provides vertical scrolling and zooming
abstract class BaseXYBTreeChart(container: SharedAxisCharts)
    extends BaseBTreeChart(container) {
  val yLower: DoubleProperty = DoubleProperty(0)
  val yUpper: DoubleProperty = DoubleProperty(0)

  this.onScroll = (event: ScrollEvent) => {
    if (event.isShiftDown) {
      if (event.isControlDown) {
        val increment = -event.getDeltaX // shifts X/Y axes: https://stackoverflow.com/questions/42429591/javafx-shiftscrollwheel-always-return-0-0
        val range = yUpper.value - yLower.value
        val mouseFrac = 1 - event.getY / height.value
        val mouseValue = yLower.value + (range * mouseFrac)
        val newRange = range * Math.pow(1.01, increment)
        yLower.value = mouseValue - (newRange * mouseFrac)
        yUpper.value = mouseValue + (newRange * (1 - mouseFrac))
      } else {
        val increment = -event.getDeltaX
        val range = yUpper.value - yLower.value
        val shift = (range / 256) * increment
        yLower.value = yLower.value + shift
        yUpper.value = yUpper.value + shift
      }
      event.consume()
    }
  }
}
