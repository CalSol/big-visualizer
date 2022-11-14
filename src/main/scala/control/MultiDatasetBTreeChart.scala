package bigvis
package control

import btree.{BTree, BTreeAggregator}

import scalafx.collections.ObservableMap
import scalafx.scene.paint.Color


// Mixin for B-tree charts that can contains multiple datasets
trait MultiDatasetBTreeChart[AggregatorType <: BTreeAggregator] { this: BaseBTreeChart =>
  // implement this, to provide the aggregator type for dataset validation
  protected val aggregatorType: AggregatorType

  protected val datasets = ObservableMap[String, (BTree[AggregatorType], Color)]()

  override def addDataset(dataset: BTreeSeries): Boolean = {
    if (dataset.tree.aggregatorType != aggregatorType) {
      return false
    }
    val tree = dataset.tree.asInstanceOf[BTree[AggregatorType]]
    datasets.put(dataset.name, (tree, ChartTools.colorForIndex(datasets.size)))
    true
  }
}


trait XYAutosizingBTreeChart[AggregatorType <: BTreeAggregator] {
  this: BaseBTreeChart with XYBTreeChart with MultiDatasetBTreeChart[AggregatorType] =>
  // implement this, to get the min and max value limits of a tree
  protected def getTreeValueLimits(tree: BTree[AggregatorType]): (Double, Double)

  datasets.onChange(
    if (yLower.getValue == yUpper.getValue) {
      val limits = datasets.toSeq.map { case (name, (tree, color)) => getTreeValueLimits(tree) }

      yLower.value = limits.map(_._1).min
      yUpper.value = limits.map(_._2).max
    }
  )
}
