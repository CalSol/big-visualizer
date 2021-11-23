package bigvis

import btree._

import javafx.scene.canvas.GraphicsContext
import javafx.scene.paint.Color


object StringChartCanvas {
  protected val AGGREGATE_ALPHA = 0.05
  protected var yval = -1
}


class StringChartCanvas extends ResizableCanvas {
  import StringChartCanvas._

  // Actual rendering functions
  protected def drawChart(gc: GraphicsContext, scale: ChartParameters,
                          sections: Seq[Seq[BTreeData[StringAggregator]]], chartColor: Color,
                          chartMetadata: ChartMetadata,
                          offset: Int): Unit = {
    gc.save()
    gc.setFill(chartColor)
    gc.setStroke(chartColor)

    val renderTime = timeExec {
      sections.foreach { section =>
        // render the aggregate ranges
        gc.save()
        gc.setFill(chartColor.deriveColor(0, 1, 1, 1.0))
        val Points = section.map {
          case node: BTreeLeaf[StringAggregator] => (node.point._1, Some(node.point._2))
          case node: BTreeAggregate[StringAggregator] => ((node.maxTime + node.minTime) / 2, node.nodeData.summary)

        }
        val allPoints = Points.map { point => scale.xValToPos(point._1)}.toArray
        val allStringpoints = Points.map { point => point._2}.toArray
        gc.setLineWidth(10)
        gc.setStroke(chartColor.deriveColor(0, 1, 0, 1))
        gc.beginPath()
        gc.moveTo(allPoints.min, scale.height/2)
        gc.lineTo(allPoints.max, scale.height/2)
        gc.stroke()
        allStringpoints.distinct match {
          case Array(Some(value)) =>
            gc.save()
            gc.translate((allPoints.min + allPoints.max)/2 - 10, scale.height/2 - 16)
            gc.rotate(-40)
            gc.fillText(value, 0, 0)
            gc.restore()
            gc.setLineWidth(1)
            gc.setStroke(chartColor.deriveColor(0, 1, 0, 1))
            gc.beginPath()
            gc.moveTo((allPoints.min + allPoints.max)/2, scale.height/2)
            gc.lineTo((allPoints.min + allPoints.max)/2, scale.height/2 - 15)
            gc.stroke()
          case _ =>
        }
        gc.restore()
      }
    }


    val totalTime = chartMetadata.nodeTime + chartMetadata.sectionTime + chartMetadata.resampleTime + renderTime
    // render debugging information
    gc.fillText(f"${totalTime * 1000}%.1f ms total    " +
      f"${chartMetadata.nodeTime * 1000}%.1f ms nodes, " +
      f"${chartMetadata.sectionTime * 1000}%.1f ms sections, " +
      f"${chartMetadata.resampleTime * 1000}%.1f ms resample, " +
      f"${renderTime * 1000}%.1f ms render    " +
      f"${chartMetadata.nodes} -> ${chartMetadata.resampledNodes} nodes",
      0, 20 + (offset * 10))

    gc.restore()
  }

  def draw(scale: ChartParameters,
           charts: Seq[(ChartDefinition, ChartMetadata, Seq[Seq[BTreeData[StringAggregator]]])]): Unit = {
    val gc = getGraphicsContext2D

    gc.clearRect(0, 0, scale.width, scale.height)

    // TODO proper scales for Y axis
    gc.fillText(s"${scale.yMax}", 0, scale.height)
    gc.fillText(s"${scale.yMax}", 0, 10)

    val renderTime = timeExec {
      charts.zipWithIndex.foreach { case ((dataset, metadata, sections), i) =>
        drawChart(gc, scale, sections, dataset.color, metadata, i)
      }
    }
    gc.fillText(f" => ${renderTime * 1000}%.1f ms total render",
      0, 20 + (charts.length * 10) + 10)
  }
}

