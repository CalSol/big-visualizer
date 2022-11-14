package bigvis

import btree._
import control.{BaseChartCanvas, ChartParameters, PerfTreeView}

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
        // render the aggregate ranges
        gc.save()
        gc.setFill(chartColor.deriveColor(0, 1, 1, AGGREGATE_ALPHA))
        val bottomPoints = section.map {
          case node: BTreeLeaf[FloatArrayAggregator] => (node.point._1, node.point._2.min)
          case node: BTreeAggregate[FloatArrayAggregator] => ((node.maxTime + node.minTime) / 2, node.nodeData.min)
        }
        val topPoints = section.map {
          case node: BTreeLeaf[FloatArrayAggregator] => (node.point._1, node.point._2.max)
          case node: BTreeAggregate[FloatArrayAggregator] => ((node.maxTime + node.minTime) / 2, node.nodeData.max)
        }
        val polygonPoints = bottomPoints ++ topPoints.reverse
        val polygonXs = polygonPoints.map { point => scale.xValToPos(point._1) }.toArray
        val polygonYs = polygonPoints.map { point => scale.yValToPos(point._2) }.toArray
        gc.fillPolygon(polygonXs, polygonYs, polygonPoints.size)
        gc.restore()
      }

      sectionedData.data.foreach { section =>
        // render the data / average lines
        val sectionPoints = section.map {
          case node: BTreeAggregate[FloatArrayAggregator] =>
            ((node.minTime + node.maxTime) / 2, node.nodeData.sum / node.nodeData.count)
          case node: BTreeLeaf[FloatArrayAggregator] =>
            node.point._2.foreach { point =>
              gc.fillOval(
                scale.xValToPos(node.point._1) - 2,
                scale.yValToPos(point) - 2,
                4, 4) // render as points
            }

            node.point._2.sum / node.point._2.length
        }

//        gc.strokePolyline(
//          sectionPoints.map(point => scale.xValToPos(point._1)).toArray,
//          sectionPoints.map(point => scale.yValToPos(point._2)).toArray,
//          sectionPoints.length)
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
