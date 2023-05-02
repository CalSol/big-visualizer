package bigvis
import java.time.format.DateTimeFormatter
import java.time.temporal.ChronoUnit
import java.time.{Instant, ZoneOffset, ZonedDateTime}
import scala.collection.mutable


trait AxisScale {
  protected val postfixFormatter: DateTimeFormatter

  // Truncates a ZonedDateTime to the previous tick
  protected def truncateDateTime(initial: ZonedDateTime): ZonedDateTime
  // Advances a ZonedDateTime to the next tick, preserving any sub-tick offset
  protected def advanceDateTime(initial: ZonedDateTime): ZonedDateTime

  // Returns the typical span, in milliseconds
  val nominalSpan: Long

  // Given a time range, returns the formatted tick labels and associated times
  // including the prior one.
  // Time zone is taken from minTime.
  def getTicks(minTime: ZonedDateTime, maxTime: ZonedDateTime): (ZonedDateTime, Seq[ZonedDateTime]) = {
    val begin = truncateDateTime(minTime)
    var next = advanceDateTime(begin)
    val ticksBuilder = mutable.ArrayBuffer[ZonedDateTime]()
    while (next.isBefore(maxTime)) {
      ticksBuilder.append(next)
      next = advanceDateTime(next)
    }
    (begin, ticksBuilder.toSeq)
  }

  // For a given time in milliseconds, returns the postfix string (this without the coarser postfix)
  def getPostfixString(dateTime: ZonedDateTime): String = {
    postfixFormatter.format(dateTime)
  }
}

trait ContextAxisScale extends AxisScale {
  protected val prefixFormatter: DateTimeFormatter

  // For a given time in milliseconds, return the prefix string (from this to coarser)
  def getPrefixString(dateTime: ZonedDateTime): String = {
    prefixFormatter.format(dateTime)
  }
}

object AxisScales {
  class Year(n: Int) extends ContextAxisScale {
    override protected val prefixFormatter: DateTimeFormatter =
      DateTimeFormatter.ofPattern("YYYY '['zzz / X']'")
    override protected val postfixFormatter: DateTimeFormatter =
      DateTimeFormatter.ofPattern("YYYY 'y'")

    override protected def truncateDateTime(initial: ZonedDateTime): ZonedDateTime =
      initial.truncatedTo(ChronoUnit.DAYS).withDayOfYear(1).withYear(initial.getYear / n * n)  // truncate to years not supported
    override protected def advanceDateTime(initial: ZonedDateTime): ZonedDateTime =
      initial.plusYears(n)

    override val nominalSpan: Long = 1000L * 60 * 60 * 24 * 365 * n
    //                               ms>s   s>m  m>hr hr>day

    override def toString = s"Years($n)"
  }

  object Month extends ContextAxisScale {
    override protected val prefixFormatter: DateTimeFormatter =
      DateTimeFormatter.ofPattern("YYYY MMM '['zzz / X']'")
    override protected val postfixFormatter: DateTimeFormatter =
      DateTimeFormatter.ofPattern("MMM")

    override protected def truncateDateTime(initial: ZonedDateTime): ZonedDateTime =
      initial.truncatedTo(ChronoUnit.DAYS).withDayOfMonth(1)  // truncate to months not supported
    override protected def advanceDateTime(initial: ZonedDateTime): ZonedDateTime =
      initial.plusMonths(1)

    override val nominalSpan: Long = 1000L * 60 * 60 * 24 * 30
    //                               ms>s   s>m  m>hr hr>day

    override def toString = s"Month"
  }

  class Days(n: Int) extends AxisScale {
    override protected val postfixFormatter: DateTimeFormatter =
      DateTimeFormatter.ofPattern("d 'd' (EEE)")

    override protected def truncateDateTime(initial: ZonedDateTime): ZonedDateTime =
      initial.truncatedTo(ChronoUnit.DAYS).withDayOfMonth(initial.getDayOfMonth / n * n)
    override protected def advanceDateTime(initial: ZonedDateTime): ZonedDateTime =
      initial.plusDays(n)

    override val nominalSpan: Long = 1000L * 60 * 60 * 24 * n
    //                               ms>s   s>m  m>hr hr>day

    override def toString = s"Days($n)"
  }

  object Day extends Days(1) with ContextAxisScale {
    override protected val prefixFormatter: DateTimeFormatter =
      DateTimeFormatter.ofPattern("YYYY MMM d (EEE) '['zzz / X']'")
  }

  class Hours(n: Int) extends AxisScale {
    override protected val postfixFormatter: DateTimeFormatter =
      DateTimeFormatter.ofPattern("HH 'h'")

    override protected def truncateDateTime(initial: ZonedDateTime): ZonedDateTime =
      initial.truncatedTo(ChronoUnit.DAYS).withHour(initial.getHour / n * n)
    override protected def advanceDateTime(initial: ZonedDateTime): ZonedDateTime =
      initial.plusHours(n)

    override val nominalSpan: Long = 1000L * 60 * 60 * n
    //                               ms>s   s>m  m>hr

    override def toString = s"Hours($n)"
  }

  object Hour extends Hours(1) with ContextAxisScale {
    override protected val prefixFormatter: DateTimeFormatter =
      DateTimeFormatter.ofPattern("YYYY MMM d (EEE)  HH 'h' '['zzz / X']'")
  }

  class Minutes(n: Int) extends AxisScale {
    override protected val postfixFormatter: DateTimeFormatter =
      DateTimeFormatter.ofPattern("mm 'm'")

    override protected def truncateDateTime(initial: ZonedDateTime): ZonedDateTime =
      initial.truncatedTo(ChronoUnit.HOURS).withMinute(initial.getMinute / n * n)
    override protected def advanceDateTime(initial: ZonedDateTime): ZonedDateTime =
      initial.plusMinutes(n)

    override val nominalSpan: Long = 1000L * 60 * n
    //                               ms>s   s>m

    override def toString = s"Minutes($n)"
  }

  object Minute extends Minutes(1) with ContextAxisScale {
    override protected val prefixFormatter: DateTimeFormatter =
      DateTimeFormatter.ofPattern("YYYY MMM d (EEE)  HH:mm '['zzz / X']'")
  }

  class Seconds(n: Int) extends AxisScale {
    override protected val postfixFormatter: DateTimeFormatter =
      DateTimeFormatter.ofPattern("ss 's'")

    override protected def truncateDateTime(initial: ZonedDateTime): ZonedDateTime =
      initial.truncatedTo(ChronoUnit.MINUTES).withSecond(initial.getSecond / n * n)
    override protected def advanceDateTime(initial: ZonedDateTime): ZonedDateTime =
      initial.plusSeconds(n)

    override val nominalSpan: Long = 1000L * n
    //                               ms>s

    override def toString = s"Seconds($n)"
  }

  object Second extends Seconds(1) with ContextAxisScale {
    override protected val prefixFormatter: DateTimeFormatter =
      DateTimeFormatter.ofPattern("YYYY MMM d (EEE)  HH:mm:ss '['zzz / X']'")
  }

  class Milliseconds(n: Int) extends AxisScale {
    override protected val postfixFormatter: DateTimeFormatter =
      DateTimeFormatter.ofPattern("SSS 'ms'")

    override protected def truncateDateTime(initial: ZonedDateTime): ZonedDateTime =
      initial.truncatedTo(ChronoUnit.SECONDS).withNano(initial.getNano / 1000000 / n * 1000000 * n)
    override protected def advanceDateTime(initial: ZonedDateTime): ZonedDateTime =
      initial.plusNanos(1000000 * n)

    override val nominalSpan: Long = n

    override def toString = s"Milliseconds($n)"
  }

  object Millisecond extends Milliseconds(1) with ContextAxisScale {
    override protected val prefixFormatter: DateTimeFormatter =
      DateTimeFormatter.ofPattern("YYYY MMM d (EEE)  HH:mm:ss.SSSS '['zzz / X']'")
  }

  protected val all = Seq(
    new Year(1000), new Year(100), new Year(10), new Year(1),
    Month,
    Day, new Hours(6),
    Hour, new Minutes(10),
    Minute, new Seconds(10),
    Second, new Milliseconds(100), new Milliseconds(10),
    Millisecond
  )

  // Picks the finest scale with a nominal span of at least the input
  def getScaleWithBestSpan(span: Long): AxisScale = {
    all.findLast(_.nominalSpan > span).getOrElse(all.head)
  }

  // Picks the context scale for the given scale - typically the one one chrono unit coarser
  // (eg, hours if the input is minutes or 10 minutes)
  def getContextScale(scale: AxisScale): ContextAxisScale = {
    val scaleIndex = all.indexOf(scale)
    val truncated = all.dropRight(all.length - scaleIndex)  // drop the input scale and all finer
    truncated.collect {
      case x: ContextAxisScale => x
    }.lastOption.getOrElse(all.head.asInstanceOf[ContextAxisScale])
  }

  // Picks the finer context scale for a given scale - the chrono unit at or finer than the next scale.
  // (eg, minutes if the input is 10 minutes, or seconds if the input is seconds)
  def getFinerScale(scale: AxisScale): ContextAxisScale = {
    val scaleIndex = all.indexOf(scale)
    val truncated = all.drop(scaleIndex + 1)  // drop the input scale and all coarser
    truncated.collectFirst {
      case x: ContextAxisScale => x
    }.getOrElse(all.last.asInstanceOf[ContextAxisScale])
  }
}
