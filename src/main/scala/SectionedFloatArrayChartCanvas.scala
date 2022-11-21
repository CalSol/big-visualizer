package bigvis

import btree._
import control.{BaseChartCanvas, ChartParameters, ChartTools, PerfTreeView, SectionedChartCanvasUtils}

import javafx.scene.canvas.GraphicsContext
import scalafx.scene.paint.Color


object SectionedFloatArrayChartCanvas {
  protected val AGGREGATE_ALPHA = 0.25
}


class SectionedFloatArrayChartCanvas extends BaseChartCanvas {
  import SectionedFloatArrayChartCanvas._

  // Actual rendering functions, returns rendering time
  protected def drawChart(gc: GraphicsContext, scale: ChartParameters,
                          sectionedData: SectionedData[FloatArrayAggregator], chartColor: Color): Double = {
    gc.save()
    gc.setFill(chartColor)
    gc.setStroke(chartColor)

    val renderTime = timeExec {
      sectionedData.data.foreach { section =>
        SectionedChartCanvasUtils.foreachSectionByNodeType[FloatArrayAggregator](section, { aggregateSection =>
          val bottomTopMeanPoints = aggregateSection.map { node =>
            (scale.xValToPos((node.maxTime + node.minTime) / 2),
                scale.yValToPos(node.nodeData.min),
                scale.yValToPos(node.nodeData.max),
                scale.yValToPos(node.nodeData.sum / node.nodeData.count))
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
        }, { leafSection =>
          val series = leafSection.map { node =>
            val nodeTime = scale.xValToPos(node.point._1)
            node.point._2.map { value =>
              (nodeTime, scale.yValToPos(value))
            }
          }.transpose
          gc.save()
          series.zipWithIndex foreach { case (points, index) =>
            val seriesColor = ChartTools.colorForSubseries(chartColor, index, series.length)
            gc.setFill(seriesColor)
            gc.setStroke(seriesColor)

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
          gc.restore()
        })
      }
    }

    gc.restore()

    renderTime
  }

  def draw(scale: ChartParameters,
           charts: Seq[(String, SectionedData[FloatArrayAggregator], Color)]): Unit = {
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
