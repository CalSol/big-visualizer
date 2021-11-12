package bigvis

import bigvis.btree.{BTree, FloatAggregator}
import de.siegmar.fastcsv.reader.CsvReader

import java.nio.file.Path
import scala.collection.mutable
import scala.jdk.CollectionConverters.{IteratorHasAsScala, ListHasAsScala}
import scala.util.Try


trait DataBuilder {
  val name: String
}


trait Parser {
  def parseCell(time: Long, value: String): Unit
  def getBuilder: DataBuilder
}


class DummyParser(val name: String) extends Parser {
  def parseCell(time: Long, value: String): Unit = {}

  override def getBuilder: DataBuilder = new DataBuilder {
    override val name = DummyParser.this.name
  }
}


class StringParser(val name: String) extends Parser with DataBuilder {
  override def toString: String = s"${getClass.getName}($name)"

  protected val dataBuilder = mutable.ArrayBuffer[(Long, String)]()

  override def parseCell(time: Long, value: String): Unit = {
    if (value.isEmpty) {
      return
    }

    dataBuilder.append((time, value))
  }

  override def getBuilder: DataBuilder = this
}


class DoubleParser(val name: String) extends Parser with DataBuilder {
  override def toString: String = s"${getClass.getName}($name)"

  protected val dataBuilder = mutable.ArrayBuffer[(Long, Double)]()

  override def parseCell(time: Long, value: String): Unit = {
    if (value.isEmpty) {
      return
    }
    dataBuilder.append((time, value.toDouble))
  }

  override def getBuilder: DataBuilder = this
}


class DoubleArrayBuilder(val name: String) extends DataBuilder {
  protected val dataBuilder = mutable.ArrayBuffer[(Long, Map[Int, Double])]()
  protected var assemblyTime: Option[Long] = None
  protected val assembly = mutable.HashMap[Int, Double]()

  class CellParser(index: Int) extends Parser {
    override def toString: String = s"${getClass.getName}($name, $index)"

    def parseCell(time: Long, value: String): Unit = {
      if (value.isEmpty) {
        return
      }
      if (assemblyTime != Some(time)) {
        if (assembly.nonEmpty) {
          dataBuilder.append((assemblyTime.get, assembly.toMap))
        }
        assemblyTime = Some(time)
        assembly.clear()
      }
      assembly.put(index, value.toDouble)
    }

    override def getBuilder: DataBuilder = DoubleArrayBuilder.this
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
    val dataTypesMap = (headers zip dataTypes).toMap

    val headerArrays = headers.groupBy(str => str.reverse.dropWhile(_.isDigit).reverse)
        .filter(_._2.length >= ARRAY_MIN_LEN)

    val headerDoubleArrays = headerArrays.filter { case (arrayPrefix, arrayElts) =>
      arrayElts.forall(dataTypesMap.getOrElse(_, None) == Some(classOf[Double]))
    }
    val doubleArraysMap = headerDoubleArrays.flatMap { case (arrayPrefix, arrayElts) =>
      val builder = new DoubleArrayBuilder(arrayPrefix)
      arrayElts.map { arrayElt =>
        val index = arrayElt.reverse.takeWhile(_.isDigit).reverse.toInt
        arrayElt -> new builder.CellParser(index)
      }
    }

    val parsers = (headers zip dataTypes) map {
      case (header, dataType) if doubleArraysMap.contains(header) =>
        doubleArraysMap(header)
      case (header, Some(dataType)) if dataType == classOf[Double] =>
        new DoubleParser(header)
      case (header, Some(dataType)) if dataType == classOf[String] =>
        new StringParser(header)
      case (header, None) =>
        new DummyParser(header)
    }

    var count: Long = 0
    val loadTime = timeExec {
      (firstRows ++ rowIter).foreach { rawRow =>
        val row = rawRow.getFields.asScala
        val time = (row.head.toDouble * 1000).toLong
        (row.tail zip parsers).foreach { case (cell, parser) =>
          parser.parseCell(time, cell)
        }
        
        count += 1
      }
    }

    println(f"loaded $count rows in $loadTime s")
  }
}
