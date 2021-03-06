package bigvis

object timeExec {
  def apply(block: => Unit): Double = {
    val t0 = System.nanoTime()
    block  // call-by-name
    val t1 = System.nanoTime()
    (t1 - t0) / 1000.0 / 1000 / 1000
  }

  def apply[T](block: => T): (Double, T) = {
    val t0 = System.nanoTime()
    val result = block  // call-by-name
    val t1 = System.nanoTime()
    ((t1 - t0) / 1000.0 / 1000 / 1000, result)
  }
}
