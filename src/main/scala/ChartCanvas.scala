package bigvis

import btree._

import javafx.scene.canvas.GraphicsContext
import javafx.scene.paint.Color


object ChartCanvas {
  protected val AGGREGATE_ALPHA = 0.33
}


class ChartCanvas extends ResizableCanvas {
  import ChartCanvas._

  // Actual rendering functions
  protected def drawChart(gc: GraphicsContext, scale: ChartParameters,
                          sections: Seq[Seq[BTreeData[FloatAggregator]]], chartColor: Color,
                          chartMetadata: ChartMetadata,
                          offset: Int): Unit = {
    gc.save()
    gc.setFill(chartColor)
    gc.setStroke(chartColor)

    val renderTime = timeExec {
      sections.foreach { section =>
        // render the aggregate ranges
        val contiguousNodeSubsections = ChunkSeq[BTreeData[FloatAggregator], BTreeData[FloatAggregator]](section, section.head, {
          case (prev: BTreeNode[FloatAggregator], curr: BTreeNode[FloatAggregator]) => (curr, false)
          case (prev: BTreeLeaf[FloatAggregator], curr: BTreeLeaf[FloatAggregator]) => (curr, false)
          case (_, curr) => (curr, true)
        })
        gc.save()
        gc.setFill(chartColor.deriveColor(0, 1, 1, AGGREGATE_ALPHA))
        contiguousNodeSubsections
            .filter(_.head.isInstanceOf[BTreeNode[FloatAggregator]])
            .asInstanceOf[Seq[Seq[BTreeNode[FloatAggregator]]]]
            .foreach { subsection =>
              val bottomPoints = subsection.map { node =>
                ((node.maxTime + node.minTime) / 2, node.nodeData.min)
              }
              val topPoints = subsection.map { node =>
                ((node.maxTime + node.minTime) / 2, node.nodeData.max)
              }
              val polygonPoints = bottomPoints ++ topPoints.reverse
              val polygonXs = polygonPoints.map{point => scale.xValToPos(point._1)}.toArray
              val polygonYs = polygonPoints.map{point => scale.yValToPos(point._2)}.toArray
              //                  gc.fillPolygon(polygonXs, polygonYs, polygonPoints.size)
            }
        gc.restore()

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
           charts: Seq[(ChartDefinition, ChartMetadata, Seq[Seq[BTreeData[FloatAggregator]]])]): Unit = {
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
