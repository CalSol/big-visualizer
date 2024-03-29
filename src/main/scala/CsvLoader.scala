package bigvis

import btree._

import bigvis.util.TupleArrayBuilder
import de.siegmar.fastcsv.reader.CsvReader

import java.nio.file.Path
import scala.collection.IterableOnce.iterableOnceExtensionMethods
import scala.collection.mutable.ArrayBuffer
import scala.collection.{SeqMap, mutable}
import scala.jdk.CollectionConverters.{IteratorHasAsScala, ListHasAsScala}
import scala.util.Try


trait DataBuilder {
  def name: String
  def makeTree(statusFn: Float => Unit): UntypedBTree
}


trait Parser {
  def parseCell(time: Long, value: String): Unit
  def getBuilder: DataBuilder
}


object Parser {
  val BTREE_LEAF_SIZE = 64
  val BTREE_NODE_SIZE = 16
}


class DummyParser(val name: String) extends Parser {
  def parseCell(time: Long, value: String): Unit = {}

  override def getBuilder: DataBuilder = new DataBuilder {
    override def name: String = DummyParser.this.name
    override def makeTree(statusFn: Float => Unit) =
      throw new IllegalArgumentException("can't create tree from DummyParser")
  }
}


class StringParser(val name: String) extends Parser with DataBuilder {
  override def toString: String = s"${getClass.getName}($name)"

  protected val dataBuilder = new TupleArrayBuilder[Long, String]()

  override def parseCell(time: Long, value: String): Unit = {
    if (value.isEmpty) {
      return
    }
    dataBuilder.addOne(time, value)
  }

  override def getBuilder: DataBuilder = this
  override def makeTree(statusFn: Float => Unit): BTree[StringAggregator.type] = {
    val tree = new BTree(StringAggregator, Parser.BTREE_LEAF_SIZE, Parser.BTREE_NODE_SIZE)
    tree.appendAll(dataBuilder.result(), statusFn)
    tree
  }
}


class FloatParser(val name: String) extends Parser with DataBuilder {
  override def toString: String = s"${getClass.getName}($name)"

  protected val dataBuilder = new TupleArrayBuilder[Long, Float]()

  override def parseCell(time: Long, value: String): Unit = {
    if (value.isEmpty) {
      return
    }
    dataBuilder.addOne(time, value.toFloat)
  }

  override def getBuilder: DataBuilder = this
  override def makeTree(statusFn: Float => Unit): BTree[FloatAggregator.type] = {
    val tree = new BTree(FloatAggregator, Parser.BTREE_LEAF_SIZE, Parser.BTREE_NODE_SIZE)
    tree.appendAll(dataBuilder.result(), statusFn)
    tree
  }
}


class FloatArrayBuilder(val name: String) extends DataBuilder {
  protected var currentElement: Option[(Long, mutable.ArrayBuilder[Float])] = None
  protected val dataBuilder = new TupleArrayBuilder[Long, Array[Float]]()
  protected var arraySize: Int = 0

  class CellParser(index: Int) extends Parser {
    override def toString: String = s"${getClass.getName}($name, $index)"

    def parseCell(time: Long, value: String): Unit = {
      if (value.isEmpty) {
        return
      }
      if (currentElement.isEmpty || currentElement.get._1 != time) {
        currentElement match {
          case Some((currentTime, currentBuffer)) =>
            arraySize = math.max(arraySize, currentBuffer.length)
            require(currentTime < time, s"data jumped back in time at $time")
            dataBuilder.addOne(currentTime, currentBuffer.result())
            currentBuffer.clear()  // reuse the buffer for memory efficiency
            currentElement = Some((time, currentBuffer))
          case None =>
            currentElement = Some((time, Array.newBuilder[Float]))
        }

      }
      if (currentElement.get._2.length != index) {
        return  // assumption: arrays must be full - partial arrays are warned at the makeTree stage
      }
      currentElement.get._2.addOne(value.toFloat)
    }

    override def getBuilder: DataBuilder = FloatArrayBuilder.this
  }

  override def makeTree(statusFn: Float => Unit): BTree[FloatArrayAggregator.type] = {
    val arrayData = dataBuilder.result().filter { case (time, data) =>
      if (data.length != arraySize) {
        println(f"${this.getClass.getSimpleName} ${this.name} discard non-full array (${data.size} / $arraySize) at $time")
      }
      data.length == arraySize
    }
    val tree = new BTree(FloatArrayAggregator, Parser.BTREE_LEAF_SIZE, Parser.BTREE_NODE_SIZE)
    tree.appendAll(arrayData, statusFn)
    tree
  }
}


case class BTreeSeries(name: String, tree: UntypedBTree)


object CsvLoader {
  val SCAN_ROWS = 16  // rows to scan to determine type of a cell
  val ARRAY_MIN_LEN = 4

  val ROWS_BETWEEN_UPDATES = 65536

  def load(path: Path)(status: String => Unit): Seq[BTreeSeries] = {
    val fileLength = path.toFile.length().toFloat

    status(s"determining types")
    val csv = CsvReader.builder().build(path)
    val rowIter = csv.iterator().asScala
    // implied that first is the timestamp
    val headers = rowIter.take(1).toSeq.head.getFields.asScala.toSeq.drop(1)
    val firstRows = rowIter.take(SCAN_ROWS).toSeq
    val firstCols = firstRows.map { row =>
      row.getFields.asScala.drop(1)
    }.transpose
    val dataTypes = firstCols.map { colData =>
      colData.filter(_.nonEmpty) match {
        case seq if seq.nonEmpty && seq.forall(str => Try(str.toDouble).isSuccess) => Some(classOf[Double])
        case seq if seq.nonEmpty && seq.forall(str => str.nonEmpty) => Some(classOf[String])
        case Seq() => None
      }
    }
    val dataTypesPairs = (headers zip dataTypes)
    val dataTypesMap = dataTypesPairs.toMap

    System.gc()

    // Infer array types by looking for things ending with numbers and checking for a common prefix
    val headerArrays = headers.groupBy(str => str.reverse.dropWhile(_.isDigit).reverse)
        .filter(_._2.length >= ARRAY_MIN_LEN)
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
    val parsers = dataTypesPairs map {
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
    status(s"reading")
    var count: Long = 0
    val loadTime = timeExec {
      val csv = CsvReader.builder().build(path)
      val rowIter = csv.iterator().asScala
      rowIter.take(1).toSeq  // drop header row
      rowIter.foreach { rawRow =>
        if (count % ROWS_BETWEEN_UPDATES == 0) {
          status(s"reading: ${(rawRow.getStartingOffset / fileLength * 100).toInt}%")
        }

        val row = rawRow.getFields.asScala
        val time = (row.head.toDouble * 1000).toLong
        (row.tail lazyZip parsers).foreach { case (cell, parser) =>
          parser.parseCell(time, cell)
        }

        count += 1
      }
    }
    println(f"loaded $path in $loadTime%.1f s")

    System.gc()

    val dataBuilders = parsers.filter(!_.isInstanceOf[DummyParser]).map(_.getBuilder).distinct
    val series = dataBuilders.map { dataBuilder =>
      BTreeSeries(dataBuilder.name, dataBuilder.makeTree(pct =>
        status(s"inserting: ${(pct * 100).toInt}% ${dataBuilder.name}")))
    }

    System.gc()

    series
  }
}
