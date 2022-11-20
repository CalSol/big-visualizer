package bigvis
package control

import btree.{BTreeAggregate, BTreeAggregator, BTreeData, BTreeLeaf}

import javafx.scene.canvas.{Canvas, GraphicsContext}
import javafx.scene.paint.Color


object RenderHelper {
  def drawContrastLine(gc: GraphicsContext, background: Color,
                       x1: Double, y1: Double, x2: Double, y2: Double): Unit = {
    gc.save()
    gc.setStroke(background)
    gc.setLineWidth(gc.getLineWidth * 5)
    gc.strokeLine(x1, y1, x2, y2)
    gc.restore()
    gc.strokeLine(x1, y1, x2, y2)
  }

  def drawContrastText(gc: GraphicsContext, background: Color, text: String,
                       x: Double, y: Double): Unit = {
    gc.save()
    gc.setStroke(background)
    gc.setLineWidth(5)
    gc.strokeText(text, x, y)
    gc.restore()
    gc.fillText(text, x, y)
  }
}


object SectionedChartCanvasUtils {
  /** Splits an input sequence of BTree nodes into aggregate-only (including resampled nodes) and leaf-only
    * sections, then runs aggregateFn on the aggregate-only sections and leafFn on the leaf-only sections.
    */
  def foreachSectionByNodeType[AggregatorType <: BTreeAggregator]
      (input: Seq[BTreeData[AggregatorType]], aggregateFn: Seq[BTreeAggregate[AggregatorType]] => Unit,
      leafFn: Seq[BTreeLeaf[AggregatorType]] => Unit): Unit = {
    ChunkSeq[BTreeData[AggregatorType], Any](input, null, {
      case (prev: BTreeAggregate[AggregatorType], next: BTreeAggregate[AggregatorType]) => (prev, false)
      case (prev: BTreeLeaf[AggregatorType], next: BTreeLeaf[AggregatorType]) => (prev, false)
      case (prev, next) => (next, true)
    }).map(section => (section.head, section))  // provide a sample element to type-check
        .foreach {
      case (head: BTreeAggregate[AggregatorType], section) =>
        aggregateFn(section.asInstanceOf[Seq[BTreeAggregate[AggregatorType]]])
      case (head: BTreeLeaf[AggregatorType], section) =>
        leafFn(section.asInstanceOf[Seq[BTreeLeaf[AggregatorType]]])
    }
  }
}


class BaseChartCanvas extends Canvas {
  override def isResizable: Boolean = true
}
