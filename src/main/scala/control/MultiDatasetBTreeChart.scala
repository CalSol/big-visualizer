package bigvis
package control

import btree.{BTree, BTreeAggregator}

import scalafx.collections.ObservableMap
import scalafx.scene.paint.Color


// Mixin for B-tree charts that can contains multiple datasets
trait MultiDatasetBTreeChart[AggregatorType <: BTreeAggregator] { this: BaseBTreeChart =>
  protected val datasets = ObservableMap[String, (BTree[AggregatorType], Color)]()
  protected val aggregatorType: AggregatorType

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
  def getTreeValueLimits(tree: BTree[AggregatorType]): (Double, Double)

  datasets.onChange(
    if (yLower.getValue == yUpper.getValue) {
      val limits = datasets.toSeq.map { case (name, (tree, color)) => getTreeValueLimits(tree) }

      yLower.value = limits.map(_._1).min
      yUpper.value = limits.map(_._2).max
    }
  )
}
