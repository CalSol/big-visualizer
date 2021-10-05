package bigvis


class CursorCanvas extends ResizableCanvas {
  def draw(scale: ChartParameters, cursorPos: Double): Unit = {
    val gc = getGraphicsContext2D

    gc.clearRect(0, 0, scale.width, scale.height)

    val cursorTime = scale.xPosToVal(cursorPos)
    gc.strokeLine(cursorPos, 0, cursorPos, scale.height)
    gc.strokeText(s"${scale.finerScale.getPostfixString(scale.dateTimeFromTimestamp(cursorTime))}",
      cursorPos, scale.height - 60)
  }
}
