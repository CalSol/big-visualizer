package bigvis

import btree._
import control.{BaseChartCanvas, ChartParameters, FloatBTreeSeries, PerfTreeView}

import javafx.scene.canvas.GraphicsContext
import javafx.scene.paint.Color


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
        // render the aggregate ranges
        gc.save()
        gc.setFill(chartColor.deriveColor(0, 1, 1, AGGREGATE_ALPHA))
        val bottomPoints = section.map {
          case node: BTreeLeaf[FloatAggregator] => (node.point._1, node.point._2)
          case node: BTreeAggregate[FloatAggregator] => ((node.maxTime + node.minTime) / 2, node.nodeData.min)

        }
        val topPoints = section.map {
          case node: BTreeLeaf[FloatAggregator] => (node.point._1, node.point._2)
          case node: BTreeAggregate[FloatAggregator] => ((node.maxTime + node.minTime) / 2, node.nodeData.max)
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
          case node: BTreeAggregate[FloatAggregator] =>
            ((node.minTime + node.maxTime) / 2, node.nodeData.sum / node.nodeData.count)
          case node: BTreeLeaf[FloatAggregator] =>
            // TODO only render at some density instead of by B-tree?
            gc.fillOval(
              scale.xValToPos(node.point._1) - 2,
              scale.yValToPos(node.point._2) - 2,
              4, 4)  // render as points
            node.point
        }

        gc.strokePolyline(
          sectionPoints.map(point => scale.xValToPos(point._1)).toArray,
          sectionPoints.map(point => scale.yValToPos(point._2)).toArray,
          sectionPoints.length)
      }
    }

    gc.restore()

    renderTime
  }

  def draw(scale: ChartParameters,
           charts: Seq[(FloatBTreeSeries, SectionedData[FloatAggregator])]): Unit = {
    val gc = getGraphicsContext2D

    gc.clearRect(0, 0, scale.width, scale.height)

    // TODO proper scales for Y axis
    gc.fillText(s"${scale.yMin}", 0, scale.height)
    gc.fillText(s"${scale.yMax}", 0, 10)

    charts.foreach { case (dataset, sections) =>
      val renderTime = drawChart(gc, scale, sections, dataset.color)
      PerfTreeView().foreach(_.updateItemRender(dataset.name, renderTime))
    }
  }
}
