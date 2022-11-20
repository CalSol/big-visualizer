package bigvis

import btree._
import control.{BaseChartCanvas, ChartParameters, PerfTreeView}

import javafx.scene.canvas.GraphicsContext
import scalafx.scene.paint.Color


object SectionedFloatChartCanvas {
  protected val AGGREGATE_ALPHA = 0.25
}


class SectionedFloatChartCanvas extends BaseChartCanvas {
  import SectionedFloatChartCanvas._

  // Actual rendering functions, returns rendering time
  protected def drawChart(gc: GraphicsContext, scale: ChartParameters,
                          sectionedData: SectionedData[FloatAggregator], chartColor: Color): Double = {
    gc.save()
    gc.setFill(chartColor)
    gc.setStroke(chartColor)

    val renderTime = timeExec {
      sectionedData.data.foreach { section =>
        // further section by all-aggregate and all-nodes, since they have different rendering parameters
        ChunkSeq[BTreeData[FloatAggregator], Any](section, null, {
          case (prev: BTreeAggregate[FloatAggregator], next: BTreeAggregate[FloatAggregator]) => (prev, false)
          case (prev: BTreeLeaf[FloatAggregator], next: BTreeLeaf[FloatAggregator]) => (prev, false)
          case (prev, next) => (next, true)
        }).map(section => (section.head, section)).foreach {
          case (head: BTreeAggregate[FloatAggregator], section) =>
            val bottomTopMeanPoints = section.map { node =>
              val nodeData = node.asInstanceOf[BTreeAggregate[FloatAggregator]].nodeData
              (scale.xValToPos((node.maxTime + node.minTime) / 2),
                  scale.yValToPos(nodeData.min),
                  scale.yValToPos(nodeData.max),
                  scale.yValToPos(nodeData.sum / nodeData.count))
            }

            // render the aggregate ranges
            gc.save()
            gc.setFill(chartColor.deriveColor(0, 1, 1, AGGREGATE_ALPHA))
            gc.fillPolygon(
              (bottomTopMeanPoints.map(_._1) ++ bottomTopMeanPoints.reverse.map(_._1)).toArray,
              (bottomTopMeanPoints.map(_._2) ++ bottomTopMeanPoints.reverse.map(_._3)).toArray,
              bottomTopMeanPoints.length * 2)
            gc.restore()

            // render the center line
            gc.strokePolyline(
              bottomTopMeanPoints.map(_._1).toArray,
              bottomTopMeanPoints.map(_._4).toArray,
              bottomTopMeanPoints.length)
          case (head: BTreeLeaf[FloatAggregator], section) =>
            val points = section.map { node =>
              (scale.xValToPos(node.minTime),
                  scale.yValToPos(node.asInstanceOf[BTreeLeaf[FloatAggregator]].point._2))
            }

            // render the center line
            gc.strokePolyline(
              points.map(_._1).toArray,
              points.map(_._2).toArray,
              points.length)

            // render individual points
            points.foreach { point =>
              gc.fillOval(
                point._1 - 2,
                point._2 - 2,
                4, 4)
            }
        }
      }
    }

    gc.restore()

    renderTime
  }

  def draw(scale: ChartParameters,
           charts: Seq[(String, SectionedData[FloatAggregator], Color)]): Unit = {
    val gc = getGraphicsContext2D

    gc.clearRect(0, 0, scale.width, scale.height)

    // TODO proper scales for Y axis
    gc.fillText(s"${scale.yMin}", 0, scale.height)
    gc.fillText(s"${scale.yMax}", 0, 10)

    charts.foreach { case (name, sections, color) =>
      val renderTime = drawChart(gc, scale, sections, color)
      PerfTreeView().foreach(_.updateItemRender(name, renderTime))
    }
  }
}
