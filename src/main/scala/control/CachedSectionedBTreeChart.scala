package bigvis
package control

import btree.{BTree, BTreeAggregator, BTreeResampler, SectionedData}
import control.BTreeChart.PX_PER_POINT

import scalafx.collections.ObservableMap

trait SectionedBTreeChart[AggregatorType <: BTreeAggregator] {
  // implement me: time break for sectioning configuration
  val timeBreak: BTree.TimestampType


  // Given a set of parameters (defining the window and resolution) and a data series (BTree),
  // returns the sectioned (broken by timeBreak if below the minimum resolution) and resampled data.
  def calculateSection(scale: ChartParameters, series: BTree[AggregatorType], name: String):
      SectionedData[AggregatorType] = {
    val minResolution = (scale.xRange.toDouble / scale.width * PX_PER_POINT).toLong

    val (nodeTime, nodes) = timeExec {
      series.getData(scale.xMin, scale.xMax, minResolution)
    }

    // resample (combine) nodes to reduce the resolution to nearer the minimum
    val (resampleTime, resampledNodes) = timeExec {
      BTreeResampler(series.aggregatorType, nodes, minResolution)
    }

    // filter nodes into break-able sections
    val (sectionTime, sectionedData) = timeExec {
      SectionedData.from(resampledNodes, timeBreak)
    }

    PerfTreeView().foreach(_.updateItemPerf(name,
      nodes.length, resampledNodes.length, nodeTime, resampleTime, sectionTime
    ))

    sectionedData
  }
}


trait CachedSectionedMultiDatasetBTreeChart[AggregatorType <: BTreeAggregator]
    extends SectionedBTreeChart[AggregatorType] { this: BaseBTreeChart with MultiDatasetBTreeChart[AggregatorType] =>
  // processed data displayed by the current window, corresponds to MultiDatasetBTreeChart.datasets
  // subclasses should listen on this to re-render the chart
  protected val cachedSections = ObservableMap[String, SectionedData[AggregatorType]]()

  def updateSections(): Unit = {
    val scale = ChartParameters(width.value.toInt, height.value.toInt,
      container.xAxis.value, yAxis.value, container.timeZone)

    val newCachedSections = datasets.map { case (name, (tree, color)) =>
      name -> calculateSection(scale, tree, name)
    }
    // must update the collection in one operation, so observable only sees one operation
    cachedSections.addAll(newCachedSections)
  }

  Seq(width, height, container.xAxis, yAxis,
    datasets).foreach { observable =>  // triggers chart re-draw
    observable.onInvalidate(updateSections())
  }
}
