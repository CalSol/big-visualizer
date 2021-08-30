package bigvis

import scala.collection.mutable

object ChunkSeq {
  // Splits arrays based on some logic applied for each pair
  def apply[ElemType, DataType](seq: Seq[ElemType], initVal: DataType,
                                fn: (DataType, ElemType) => (DataType, Boolean)): Seq[Seq[ElemType]] = {
    val outputBuilder = mutable.ArrayBuffer[Seq[ElemType]]()
    val elemBuilder = mutable.ArrayBuffer[ElemType]()
    if (seq.isEmpty) {
      Seq()
    } else {
      var prevElem = seq.head
      var prevData = initVal
      elemBuilder.append(prevElem)
      seq.tail.foreach { elem =>
        val (newData, split) = fn(prevData, elem)
        if (split) {
          outputBuilder.append(elemBuilder.toSeq)
          elemBuilder.clear()
          elemBuilder.append(elem)
        } else {
          elemBuilder.append(elem)
        }
        prevElem = elem
        prevData = newData
      }
      outputBuilder.append(elemBuilder.toSeq)
    }
    outputBuilder.toSeq
  }
}
