package bigvis
import java.time.format.DateTimeFormatter
import java.time.temporal.ChronoUnit
import java.time.{Instant, ZoneOffset, ZonedDateTime}
import scala.collection.mutable


trait AxisScale {
  protected val prefixFormatter: DateTimeFormatter
  protected val postfixFormatter: DateTimeFormatter

  // Truncates a ZonedDateTime to the previous tick
  protected def truncateDateTime(initial: ZonedDateTime): ZonedDateTime
  // Advances a ZonedDateTime to the next tick, preserving any sub-tick offset
  protected def advanceDateTime(initial: ZonedDateTime): ZonedDateTime

  // Returns the typical span, in milliseconds
  def nominalSpan: Long

  // Given a time range, returns the formatted tick labels and associated times
  // including the prior one
  def getTicks(minTime: Long, maxTime: Long): (Long, Seq[Long]) = {
    val time = ZonedDateTime.ofInstant(Instant.ofEpochMilli(minTime), ZoneOffset.UTC)
    val begin = truncateDateTime(time)
    var next = advanceDateTime(begin)
    val ticksBuilder = mutable.ArrayBuffer[Long]()
    while (next.toEpochSecond * 1000 < maxTime) {
      ticksBuilder.append(next.toEpochSecond * 1000)
      next = advanceDateTime(next)
    }
    (begin.toEpochSecond * 1000, ticksBuilder.toSeq)
  }

  // For a given time in milliseconds, return the prefix string (from this to coarser)
  def getPrefixString(time: Long): String = {
    val dateTime = ZonedDateTime.ofInstant(Instant.ofEpochMilli(time), ZoneOffset.UTC)
    prefixFormatter.format(dateTime)
  }

  // For a given time in milliseconds, returns the postfix string (this without the coarser postfix)
  def getPostfixString(time: Long): String = {
    val dateTime = ZonedDateTime.ofInstant(Instant.ofEpochMilli(time), ZoneOffset.UTC)
    postfixFormatter.format(dateTime)
  }
}

object AxisScales {
  object Year extends AxisScale {
    override protected val prefixFormatter: DateTimeFormatter = DateTimeFormatter.ofPattern("YYYY '['X']'")
    override protected val postfixFormatter: DateTimeFormatter = DateTimeFormatter.ofPattern("YYYY 'y'")

    override protected def truncateDateTime(initial: ZonedDateTime): ZonedDateTime =
      initial.truncatedTo(ChronoUnit.DAYS).withDayOfYear(1)  // truncate to years not supported
    override protected def advanceDateTime(initial: ZonedDateTime): ZonedDateTime =
      initial.plusYears(1)

    override def nominalSpan: Long = 1000 * 60 * 60 * 24 * 365
    //                               ms>s   s>m  m>hr hr>day
  }

  object Month extends AxisScale {
    override protected val prefixFormatter: DateTimeFormatter = DateTimeFormatter.ofPattern("YYYY MMM '['X']'")
    override protected val postfixFormatter: DateTimeFormatter = DateTimeFormatter.ofPattern("MMM")

    override protected def truncateDateTime(initial: ZonedDateTime): ZonedDateTime =
      initial.truncatedTo(ChronoUnit.DAYS).withDayOfMonth(1)  // truncate to months not supported
    override protected def advanceDateTime(initial: ZonedDateTime): ZonedDateTime =
      initial.plusMonths(1)

    override def nominalSpan: Long = 1000 * 60 * 60 * 24 * 30
    //                               ms>s   s>m  m>hr hr>day
  }

  object Day extends AxisScale {
    override protected val prefixFormatter: DateTimeFormatter = DateTimeFormatter.ofPattern("YYYY MMM d '['X']'")
    override protected val postfixFormatter: DateTimeFormatter = DateTimeFormatter.ofPattern("d 'd'")

    override protected def truncateDateTime(initial: ZonedDateTime): ZonedDateTime =
      initial.truncatedTo(ChronoUnit.DAYS)
    override protected def advanceDateTime(initial: ZonedDateTime): ZonedDateTime =
      initial.plusDays(1)

    override def nominalSpan: Long = 1000 * 60 * 60 * 24
    //                               ms>s   s>m  m>hr hr>day
  }

  object Hour extends AxisScale {
    override protected val prefixFormatter: DateTimeFormatter = DateTimeFormatter.ofPattern("YYYY MMM d  HH 'h' '['X']'")
    override protected val postfixFormatter: DateTimeFormatter = DateTimeFormatter.ofPattern("HH 'h'")

    override protected def truncateDateTime(initial: ZonedDateTime): ZonedDateTime =
      initial.truncatedTo(ChronoUnit.HOURS)
    override protected def advanceDateTime(initial: ZonedDateTime): ZonedDateTime =
      initial.plusHours(1)

    override def nominalSpan: Long = 1000 * 60 * 60
    //                               ms>s   s>m  m>hr
  }

  object Minute extends AxisScale {
    override protected val prefixFormatter: DateTimeFormatter = DateTimeFormatter.ofPattern("YYYY MMM d  HH:mm '['X']'")
    override protected val postfixFormatter: DateTimeFormatter = DateTimeFormatter.ofPattern("mm 'm'")

    override protected def truncateDateTime(initial: ZonedDateTime): ZonedDateTime =
      initial.truncatedTo(ChronoUnit.MINUTES)
    override protected def advanceDateTime(initial: ZonedDateTime): ZonedDateTime =
      initial.plusMinutes(1)

    override def nominalSpan: Long = 1000 * 60
    //                               ms>s   s>m
  }

  object Second extends AxisScale {
    override protected val prefixFormatter: DateTimeFormatter = DateTimeFormatter.ofPattern("YYYY MMM d  HH:mm:ss '['X']'")
    override protected val postfixFormatter: DateTimeFormatter = DateTimeFormatter.ofPattern("ss 's'")

    override protected def truncateDateTime(initial: ZonedDateTime): ZonedDateTime =
      initial.truncatedTo(ChronoUnit.SECONDS)
    override protected def advanceDateTime(initial: ZonedDateTime): ZonedDateTime =
      initial.plusSeconds(1)

    override def nominalSpan: Long = 1000
    //                               ms>s
  }

  val all = Seq(Year, Month, Day, Hour, Minute, Second)
}
