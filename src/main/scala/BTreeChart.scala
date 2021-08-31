package bigvis

import bigvis.btree.{BTree, BTreeData, BTreeLeaf, BTreeNode, FloatAggregate}
import javafx.scene.canvas.Canvas
import javafx.scene.layout.StackPane
import scalafx.beans.property.{DoubleProperty, LongProperty}

import java.time.format.DateTimeFormatter
import java.time.temporal.ChronoUnit
import java.time.{Instant, ZoneOffset, ZonedDateTime}
import scala.collection.mutable


trait AxisScale {
  protected val prefixFormatter: DateTimeFormatter
  protected val postfixFormatter: DateTimeFormatter

  // Truncates a ZonedDateTime to the previous tick
  protected def truncateDateTime(initial: ZonedDateTime): ZonedDateTime
  // Advances a ZonedDateTime to the next tick, preserving any sub-tick offset
  protected def advanceDateTime(initial: ZonedDateTime): ZonedDateTime

  // Returns the typical span, in milliseconds
  def nominalSpan: Long

  // Given a time range, returns the formatted tick labels and associated times
  // including the prior one
  def getTicks(minTime: Long, maxTime: Long): (Long, Seq[Long]) = {
    val time = ZonedDateTime.ofInstant(Instant.ofEpochMilli(minTime), ZoneOffset.UTC)
    val begin = truncateDateTime(time)
    var next = advanceDateTime(begin)
    val ticksBuilder = mutable.ArrayBuffer[Long]()
    while (next.toEpochSecond * 1000 < maxTime) {
      ticksBuilder.append(next.toEpochSecond * 1000)
      next = advanceDateTime(next)
    }
    (begin.toEpochSecond * 1000, ticksBuilder.toSeq)
  }

  // For a given time in milliseconds, return the prefix string (from this to coarser)
  def getPrefixString(time: Long): String = {
    val dateTime = ZonedDateTime.ofInstant(Instant.ofEpochMilli(time), ZoneOffset.UTC)
    prefixFormatter.format(dateTime)
  }

  // For a given time in milliseconds, returns the postfix string (this without the coarser postfix)
  def getPostfixString(time: Long): String = {
    val dateTime = ZonedDateTime.ofInstant(Instant.ofEpochMilli(time), ZoneOffset.UTC)
    postfixFormatter.format(dateTime)
  }
}

object AxisScales {
  object Year extends AxisScale {
    override protected val prefixFormatter: DateTimeFormatter = DateTimeFormatter.ofPattern("YYYY '['X']'")
    override protected val postfixFormatter: DateTimeFormatter = DateTimeFormatter.ofPattern("YYYY 'y'")

    override protected def truncateDateTime(initial: ZonedDateTime): ZonedDateTime =
      initial.truncatedTo(ChronoUnit.DAYS).withDayOfYear(1)  // truncate to years not supported
    override protected def advanceDateTime(initial: ZonedDateTime): ZonedDateTime =
      initial.plusYears(1)

    override def nominalSpan: Long = 1000 * 60 * 60 * 24 * 365
    //                               ms>s   s>m  m>hr hr>day
  }

  object Month extends AxisScale {
    override protected val prefixFormatter: DateTimeFormatter = DateTimeFormatter.ofPattern("YYYY MMM '['X']'")
    override protected val postfixFormatter: DateTimeFormatter = DateTimeFormatter.ofPattern("MMM")

    override protected def truncateDateTime(initial: ZonedDateTime): ZonedDateTime =
      initial.truncatedTo(ChronoUnit.DAYS).withDayOfMonth(1)  // truncate to months not supported
    override protected def advanceDateTime(initial: ZonedDateTime): ZonedDateTime =
      initial.plusMonths(1)

    override def nominalSpan: Long = 1000 * 60 * 60 * 24 * 30
    //                               ms>s   s>m  m>hr hr>day
  }

  object Day extends AxisScale {
    override protected val prefixFormatter: DateTimeFormatter = DateTimeFormatter.ofPattern("YYYY MMM d '['X']'")
    override protected val postfixFormatter: DateTimeFormatter = DateTimeFormatter.ofPattern("d 'd'")

    override protected def truncateDateTime(initial: ZonedDateTime): ZonedDateTime =
      initial.truncatedTo(ChronoUnit.DAYS)
    override protected def advanceDateTime(initial: ZonedDateTime): ZonedDateTime =
      initial.plusDays(1)

    override def nominalSpan: Long = 1000 * 60 * 60 * 24
    //                               ms>s   s>m  m>hr hr>day
  }

  object Hour extends AxisScale {
    override protected val prefixFormatter: DateTimeFormatter = DateTimeFormatter.ofPattern("YYYY MMM d  HH 'h' '['X']'")
    override protected val postfixFormatter: DateTimeFormatter = DateTimeFormatter.ofPattern("HH 'h'")

    override protected def truncateDateTime(initial: ZonedDateTime): ZonedDateTime =
      initial.truncatedTo(ChronoUnit.HOURS)
    override protected def advanceDateTime(initial: ZonedDateTime): ZonedDateTime =
      initial.plusHours(1)

    override def nominalSpan: Long = 1000 * 60 * 60
    //                               ms>s   s>m  m>hr
  }

  object Minute extends AxisScale {
    override protected val prefixFormatter: DateTimeFormatter = DateTimeFormatter.ofPattern("YYYY MMM d  HH:mm '['X']'")
    override protected val postfixFormatter: DateTimeFormatter = DateTimeFormatter.ofPattern("mm 'm'")

    override protected def truncateDateTime(initial: ZonedDateTime): ZonedDateTime =
      initial.truncatedTo(ChronoUnit.MINUTES)
    override protected def advanceDateTime(initial: ZonedDateTime): ZonedDateTime =
      initial.plusMinutes(1)

    override def nominalSpan: Long = 1000 * 60
    //                               ms>s   s>m
  }

  object Second extends AxisScale {
    override protected val prefixFormatter: DateTimeFormatter = DateTimeFormatter.ofPattern("YYYY MMM d  HH:mm:ss '['X']'")
    override protected val postfixFormatter: DateTimeFormatter = DateTimeFormatter.ofPattern("ss 's'")

    override protected def truncateDateTime(initial: ZonedDateTime): ZonedDateTime =
      initial.truncatedTo(ChronoUnit.SECONDS)
    override protected def advanceDateTime(initial: ZonedDateTime): ZonedDateTime =
      initial.plusSeconds(1)

    override def nominalSpan: Long = 1000
    //                               ms>s
  }

  val all = Seq(Year, Month, Day, Hour, Minute, Second)
}

// A JavaFX widget that does lean and mean plotting without the CSS bloat that kills performance
// Inspired by:
// charting: https://dlsc.com/2015/06/16/javafx-tip-20-a-lot-to-show-use-canvas/
// custom controls: https://stackoverflow.com/questions/43808639/how-to-create-totally-custom-javafx-control-or-how-to-create-pane-with-dynamic
class BTreeChart(data: BTree[FloatAggregate, Float], timeBreak: Long) extends StackPane {
  val xLower: LongProperty = LongProperty(data.minTime)
  val xUpper: LongProperty = LongProperty(data.maxTime)

  val yLower: DoubleProperty = DoubleProperty(data.rootData.min)
  val yUpper: DoubleProperty = DoubleProperty(data.rootData.max)

  class ResizableCanvas extends Canvas {
    widthProperty.addListener(evt => draw())
    heightProperty.addListener(evt => draw())
    xLower.addListener(evt => draw())
    xUpper.addListener(evt => draw())
    yLower.addListener(evt => draw())
    yUpper.addListener(evt => draw())

    override def isResizable: Boolean = true

    def draw(): Unit = {
      val gc = getGraphicsContext2D
      val width = getWidth
      val height = getHeight

      gc.clearRect(0, 0, width, height)

      gc.fillText(s"${yLower.value}", 0, height)
      gc.fillText(s"${yUpper.value}", 0, 10)

      val xBottom = xLower.value
      val xScale = width / (xUpper.value - xLower.value)
      val yTop = yUpper.value
      val yScale = height / (yUpper.value - yLower.value)

      // select the ticks where there is at most one tick per 64px
      val tickPixels = AxisScales.all.reverse.map { scale => (scale, scale.nominalSpan * xScale) }
      val sufficientTicks = tickPixels.filter(_._2 > 64)
      val tickScale = sufficientTicks.headOption.map(_._1).getOrElse(AxisScales.all.head)

      // draw one-level-coarser (context) ticks
      val tickScaleIndex = AxisScales.all.indexOf(tickScale)
      if (tickScaleIndex > 0) {
        val contextScale = AxisScales.all(tickScaleIndex - 1)
        val (priorContextTime, contextTimes) = contextScale.getTicks(xLower.value, xUpper.value)
        contextTimes.foreach { tickTime =>
          val position = (tickTime - xBottom) * xScale
          gc.strokeLine(position, height - 20, position, height)
        }
        (xLower.value +: (contextTimes :+ xUpper.value)).sliding(2).foreach { case Seq(curr, next) =>
          val position = ((curr + next) / 2 - xBottom) * xScale
          gc.fillText(contextScale.getPrefixString(curr), position, height - 10)  // TODO anchor center
        }
      }

      // draw gridlines and ticks
      val (priorTime, tickTimes) = tickScale.getTicks(xLower.value, xUpper.value)
      tickTimes.foreach { tickTime =>
        val position = (tickTime - xBottom) * xScale
        gc.strokeLine(position, height - 30, position, height - 20)
        gc.fillText(tickScale.getPostfixString(tickTime), position, height - 30)  // TODO anchor center
      }

      // get nodes for the current level of resolution
      val range = xUpper.value - xLower.value
      val (nodeTime, nodes) = timeExec {
        data.getData(xLower.value, xUpper.value, (range / width).toLong)
      }

      // filter nodes into break-able sections
      val (sectionTime, sections) = timeExec {
        ChunkSeq(nodes, xLower.value, (prevTime: Long, elem: BTreeData[FloatAggregate, Float]) => {
          elem match {
            case node: BTreeNode[FloatAggregate, Float] =>
              (node.maxTime, node.minTime > prevTime + timeBreak)
            case node: BTreeLeaf[FloatAggregate, Float] => // TODO return individual data points
              (node.point._1, node.point._1 > prevTime + timeBreak)
          }
        })
      }

      // render the nodes
      val renderTime = timeExec {
        sections.foreach { section =>
          val sectionPoints = section.map {
            case node: BTreeNode[FloatAggregate, Float] =>
              ((node.minTime + node.maxTime) / 2, node.nodeData.sum / node.nodeData.count)
            case node: BTreeLeaf[FloatAggregate, Float] =>
              node.point
          }

          gc.strokePolyline(
            sectionPoints.map(point => (point._1 - xBottom) * xScale).toArray,
            sectionPoints.map(point => (yTop - point._2) * yScale).toArray,
            sectionPoints.length)
        }
      }

      // render debugging information
      gc.fillText(s"${nodes.length} nodes, ${sections.length} sections", 0, 20)
      gc.fillText(f"${nodeTime * 1000}%.1f ms nodes, " +
          f"${sectionTime * 1000}%.1f ms sections, " +
          f"${renderTime * 1000}%.1f ms render",
        0, 30)
    }
  }

  val canvas = new ResizableCanvas()
  getChildren.add(canvas)
  canvas.widthProperty().bind(widthProperty())
  canvas.heightProperty().bind(heightProperty())

  override protected def layoutChildren(): Unit = {
    canvas.draw()
  }
}
