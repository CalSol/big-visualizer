package bigvis

import scala.collection.mutable

object ChunkSeq {
  /** Splits arrays based on some logic applied for each pair.
   * Split for the first element is ignored.
   *
   * @param initVal the prev to pass into the fn call on the first element, can be a dummy value
   * @param fn function that takes in:
   *           prev: the initVal (on the first element) or the next value (from the previous iteration)
   *           elem: the seq (input) element being processed
   *         and returns:
   *           next: the prev value to pass into the next iteration
   *           split: whether this element begins a new sub-sequence
   */

  def apply[ElemType, DataType](seq: Seq[ElemType], initVal: DataType,
                                fn: (DataType, ElemType) => (DataType, Boolean)): Seq[Seq[ElemType]] = {
    val outputBuilder = mutable.ArrayBuffer[Seq[ElemType]]()
    val elemBuilder = mutable.ArrayBuffer[ElemType]()  // invariant: always non-empty during processing
    if (seq.isEmpty) {
      Seq()
    } else {
      elemBuilder.append(seq.head)
      var prevData = fn(initVal, seq.head)._1
      seq.tail.foreach { elem =>
        val (newData, split) = fn(prevData, elem)
        if (split) {
          outputBuilder.append(elemBuilder.toSeq)
          elemBuilder.clear()
        }
        elemBuilder.append(elem)
        prevData = newData
      }
      outputBuilder.append(elemBuilder.toSeq)
    }
    outputBuilder.toSeq
  }
}
