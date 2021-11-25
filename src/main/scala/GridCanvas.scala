package bigvis

import javafx.scene.canvas.GraphicsContext
import javafx.scene.paint.Color

import java.time.ZonedDateTime


object GridCanvas {
  protected val GRIDLINE_ALPHA = 0.25
  protected val CONTEXT_GRIDLINE_WIDTH = 2
}


class GridCanvas extends ResizableCanvas {
  import GridCanvas._

  protected def drawGridlines(gc: GraphicsContext, scale: ChartParameters,
                              tickTimes: Seq[ZonedDateTime], contextTimes: Seq[ZonedDateTime]): Unit = {
    gc.save()
    gc.setStroke(gc.getStroke.asInstanceOf[Color].deriveColor(0, 1, 1, GRIDLINE_ALPHA))

    // draw the context gridlines
    gc.save()
    gc.setLineWidth(CONTEXT_GRIDLINE_WIDTH)
    contextTimes.foreach { tickTime =>
      val position = scale.xValToPos(scale.timestampFromDateTime(tickTime))
      gc.strokeLine(position, 0, position, scale.height)
    }
    gc.restore()

    // draw the ticklines
    tickTimes.foreach { tickTime =>
      val position = scale.xValToPos(scale.timestampFromDateTime(tickTime))
      gc.strokeLine(position, 0, position, scale.height)
    }

    gc.restore()
  }


  protected def drawValueLines(gc: GraphicsContext, scale: ChartParameters): Unit = {
    gc.save()
    gc.setStroke(gc.getStroke.asInstanceOf[Color].deriveColor(0, 1, 1, GRIDLINE_ALPHA))
    gc.setLineWidth(CONTEXT_GRIDLINE_WIDTH)
    val v_interval = List.range(0,scale.height,64)
    v_interval.foreach{ value =>
      gc.strokeLine(0,value,scale.width,value)
//      println(s"DEBUG: value: $value")
      RenderHelper.drawContrastText(gc, ChartCommon.CONTRAST_BACKGROUND, ((scale.yPosToVal(value) * 100).round / 100.toDouble).toString,
        20, value)
    }
    gc.restore()
  }

  protected def drawRulers(gc: GraphicsContext, scale: ChartParameters,
                           priorContextTime: ZonedDateTime,
                           tickTimes: Seq[ZonedDateTime], contextTimes: Seq[ZonedDateTime]): Unit = {
    gc.save()
    gc.setLineWidth(CONTEXT_GRIDLINE_WIDTH)
    contextTimes.foreach { tickTime =>
      val position = scale.xValToPos(scale.timestampFromDateTime(tickTime))
      RenderHelper.drawContrastLine(gc, ChartCommon.CONTRAST_BACKGROUND,
        position, scale.height - 20,
        position, scale.height)
    }
    gc.restore()

    // for positioning the fenceposts
    val paddedContextPositions = scale.xMin +: (contextTimes.map(scale.timestampFromDateTime) :+ scale.xMax)
    // for the actual labels - this goes between the fenceposts so has one less entry
    val paddedContextLabels = priorContextTime +: contextTimes

    (paddedContextPositions.sliding(2) zip paddedContextLabels).foreach { case (Seq(currPos, nextPos), label) =>
      val position = scale.xValToPos((currPos + nextPos) / 2)
      // TODO anchor center
      RenderHelper.drawContrastText(gc, ChartCommon.CONTRAST_BACKGROUND, scale.contextScale.getPrefixString(label),
        position, scale.height - 10)
    }

    // draw tick ruler
    tickTimes.foreach { tickTime =>
      val position = scale.xValToPos(scale.timestampFromDateTime(tickTime))
      RenderHelper.drawContrastLine(gc, ChartCommon.CONTRAST_BACKGROUND,
        position, scale.height - 30,
        position, scale.height - 20)
      RenderHelper.drawContrastText(gc, ChartCommon.CONTRAST_BACKGROUND, scale.tickScale.getPostfixString(tickTime),
        position + 4, scale.height - 20)
    }
  }

  def draw(scale: ChartParameters): Unit = {
    val gc = getGraphicsContext2D

    gc.clearRect(0, 0, scale.width, scale.height)

    val (priorTime, tickTimes) = scale.tickScale.getTicks(
      scale.dateTimeFromTimestamp(scale.xMin), scale.dateTimeFromTimestamp(scale.xMax))
    val (priorContextTime, contextTimes) = scale.contextScale.getTicks(
      scale.dateTimeFromTimestamp(scale.xMin), scale.dateTimeFromTimestamp(scale.xMax))

    drawGridlines(gc, scale, tickTimes, contextTimes)
    drawRulers(gc, scale, priorContextTime, tickTimes, contextTimes)
    drawValueLines(gc,scale)
  }
}
