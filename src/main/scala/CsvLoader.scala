package bigvis

import btree._

import de.siegmar.fastcsv.reader.CsvReader

import java.nio.file.Path
import scala.collection.mutable
import scala.jdk.CollectionConverters.{IteratorHasAsScala, ListHasAsScala}
import scala.util.Try


trait DataBuilder {
  def name: String
  def makeTree: UntypedBTree
}


trait Parser {
  def parseCell(time: Long, value: String): Unit
  def getBuilder: DataBuilder
}


object Parser {
  val BTREE_NODE_SIZE = 16
}


class DummyParser(val name: String) extends Parser {
  def parseCell(time: Long, value: String): Unit = {}

  override def getBuilder: DataBuilder = new DataBuilder {
    override def name: String = DummyParser.this.name
    override def makeTree = throw new IllegalArgumentException("can't create tree from DummyParser")
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
  override def makeTree: BTree[StringAggregator] = {
    val tree = new BTree(StringAggregator.aggregator, Parser.BTREE_NODE_SIZE)
    tree.appendAll(dataBuilder)
    tree
  }
}


class FloatParser(val name: String) extends Parser with DataBuilder {
  override def toString: String = s"${getClass.getName}($name)"

  protected val dataBuilder = mutable.ArrayBuffer[(Long, Float)]()

  override def parseCell(time: Long, value: String): Unit = {
    if (value.isEmpty) {
      return
    }
    dataBuilder.append((time, value.toFloat))
  }

  override def getBuilder: DataBuilder = this
  override def makeTree: BTree[FloatAggregator] = {
    val tree = new BTree(FloatAggregator.aggregator, Parser.BTREE_NODE_SIZE)
    tree.appendAll(dataBuilder)
    tree
  }
}


class FloatArrayBuilder(val name: String) extends DataBuilder {
  protected val dataBuilder = mutable.ArrayBuffer[(Long, Map[Int, Float])]()
  protected var assemblyTime: Option[Long] = None
  protected val assembly = mutable.HashMap[Int, Float]()

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
      assembly.put(index, value.toFloat)
    }

    override def getBuilder: DataBuilder = FloatArrayBuilder.this
  }

  override def makeTree: BTree[BTreeAggregator] = {
    // TODO implement me
    new BTree(FloatAggregator.aggregator, Parser.BTREE_NODE_SIZE)
  }
}


case class BTreeSeries(name: String, tree: UntypedBTree)


object CsvLoader {
  val SCAN_ROWS = 16  // rows to scan to determine type of a cell
  val ARRAY_MIN_LEN = 4

  val ROWS_BETWEEN_UPDATES = 65536

  def load(path: Path)(status: String => Unit): Seq[BTreeSeries] = {
    status(s"determining types")
    val fileLength = path.toFile.length().toFloat

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

    // Infer array types by looking for things ending with numbers and checking for a common prefix
//    val headerArrays = headers.groupBy(str => str.reverse.dropWhile(_.isDigit).reverse)
//        .filter(_._2.length >= ARRAY_MIN_LEN)
    val headerArrays = Map[String, Seq[String]]()

    val headerDoubleArrays = headerArrays.filter { case (arrayPrefix, arrayElts) =>
      arrayElts.forall(dataTypesMap.getOrElse(_, None) == Some(classOf[Double]))
    }
    val doubleArraysMap = headerDoubleArrays.flatMap { case (arrayPrefix, arrayElts) =>
      val builder = new FloatArrayBuilder(arrayPrefix)
      arrayElts.map { arrayElt =>
        val index = arrayElt.reverse.takeWhile(_.isDigit).reverse.toInt
        arrayElt -> new builder.CellParser(index)
      }
    }

    // Build up the parsers, in the same order as the data they will parse (except the first time element)
    val parsers = (headers zip dataTypes) map {
      case (header, dataType) if doubleArraysMap.contains(header) =>
        doubleArraysMap(header)
      case (header, Some(dataType)) if dataType == classOf[Double] =>
        new FloatParser(header)
      case (header, Some(dataType)) if dataType == classOf[String] =>
        new StringParser(header)
      case (header, None) =>
        new DummyParser(header)
    }

    // Actually read the CSVs
    var count: Long = 0
    val loadTime = timeExec {
      (firstRows ++ rowIter).foreach { rawRow =>
        if (count % ROWS_BETWEEN_UPDATES == 0) {
          status(s"reading: ${(rawRow.getStartingOffset / fileLength * 100).toInt}%")
        }

        val row = rawRow.getFields.asScala
        val time = (row.head.toDouble * 1000).toLong
        (row.tail zip parsers).foreach { case (cell, parser) =>
          parser.parseCell(time, cell)
        }
        rawRow.getStartingOffset

        count += 1
      }
    }

    val dataBuilders = parsers.filter(!_.isInstanceOf[DummyParser]).map(_.getBuilder).distinct
    dataBuilders.toSeq.map { dataBuilder =>
      status(s"inserting: ${dataBuilder.name}")
      BTreeSeries(dataBuilder.name, dataBuilder.makeTree)
    }
  }
}
