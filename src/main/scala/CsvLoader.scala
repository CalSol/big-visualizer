package bigvis

import bigvis.btree.{BTree, FloatAggregator}
import de.siegmar.fastcsv.reader.CsvReader

import java.nio.file.Path
import scala.collection.mutable
import scala.jdk.CollectionConverters.{IteratorHasAsScala, ListHasAsScala}
import scala.util.Try


trait Parser {
  def parseCell(time: Long, value: String): Unit
}


class StringParser extends Parser {
  protected val dataBuilder = mutable.ArrayBuffer[(Long, String)]()

  def parseCell(time: Long, value: String): Unit = {
    dataBuilder.append((time, value))
  }
}


class DoubleParser extends Parser {
  protected val dataBuilder = mutable.ArrayBuffer[(Long, Double)]()

  def parseCell(time: Long, value: String): Unit = {
    dataBuilder.append((time, value.toDouble))
  }
}


class DoubleArrayBuilder {
  protected val dataBuilder = mutable.ArrayBuffer[(Long, Map[Int, Double])]()
  protected var assemblyTime: Option[Long] = None
  protected val assembly = mutable.HashMap[Int, Double]()

  class CellParser(index: Int) extends Parser {
    def parseCell(time: Long, value: String): Unit = {
      if (assemblyTime != Some(time)) {
        if (assembly.nonEmpty) {
          dataBuilder.append((assemblyTime.get, assembly.toMap))
        }
        assemblyTime = Some(time)
        assembly.clear()
      }
      assembly.put(index, value.toDouble)
    }
  }
}


object CsvLoader {
  val SCAN_ROWS = 16  // rows to scan to determine type of a cell
  val ARRAY_MIN_LEN = 4

  def load(path: Path): Unit = {
    val csv = CsvReader.builder().build(path)
    val rowIter = csv.iterator().asScala
    // implied that first is the timestamp
    val headers = rowIter.take(1).toSeq.head.getFields.asScala.drop(1)
    val firstRows = csv.iterator().asScala.take(SCAN_ROWS).toSeq
    val firstCols = firstRows.map { row =>
      row.getFields.asScala.drop(1)
    }.transpose

    val dataTypes = firstCols.map { colData =>
      colData.filter(_.nonEmpty) match {
        case seq if seq.nonEmpty && seq.forall(str => Try(str.toDouble).isSuccess) => Some(classOf[Double])
        case seq if seq.nonEmpty => Some(classOf[String])
        case Seq() => None
      }
    }

    val potentialArrays = headers.groupBy(str => str.reverse.dropWhile(_.isDigit).reverse)
        .filter(_._2.length >= ARRAY_MIN_LEN)


    println(potentialArrays)
  }
}
