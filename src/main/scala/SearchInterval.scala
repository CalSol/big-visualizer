package bigvis

import scala.collection.Searching.{Found, InsertionPoint}


object SearchInterval {
  sealed trait SearchIntervalResult[T] {
    def index(): Int
  }
  case class ContainedIn[T](index: Int) extends SearchIntervalResult[T]
  // The closest interval is before the search point, at the index (distance is always positive)
  case class NearestBefore[T](index: Int, distance: T) extends SearchIntervalResult[T]
  // The closest interval is after the search point, at the index (distance is always positive)
  case class NearestAfter[T](index: Int, distance: T) extends SearchIntervalResult[T]


  /** Given a data containing intervals, returns the index of the interval containing the search point,
   * or the index nearest the search point if it's within some tolerance.
   */
  def apply[T](data: Seq[(T, T)], searchPoint: T, tolerance: T)
              (implicit num: Numeric[T]): Option[SearchIntervalResult[T]] = {
    import num._

    // Assumption: input is a sorted list, intervals do not overlap
    data.search((searchPoint, searchPoint))(Ordering.by(_._1)) match {  // sort by interval begin
      case Found(foundIndex) =>
        Some(ContainedIn(foundIndex))
      case InsertionPoint(insertionPoint) =>
        val prevOption = if (insertionPoint > 0) {
          val prevPoint = data(insertionPoint - 1)
          if (prevPoint._1 <= searchPoint && searchPoint <= prevPoint._2) {
            Some(ContainedIn(insertionPoint - 1))  // prefer contained section
          } else if (searchPoint - prevPoint._2 <= tolerance) {  // filter by distance tolerance
            Some(NearestBefore(insertionPoint - 1, searchPoint - prevPoint._2))
          } else {
            None
          }
        } else {
          None
        }
        val nextOption = if (insertionPoint < data.length) {
          val nextPoint = data(insertionPoint)
          if (nextPoint._1 - searchPoint <= tolerance) {  // filter by distance tolerance
            Some(NearestAfter(insertionPoint, nextPoint._1 - searchPoint))
          } else {
            None
          }
        } else {
          None
        }

        (prevOption, nextOption) match {  // if both prev and next, select the best (by contained, then distance)
          case (Some(prev: ContainedIn[T]), _) => Some(prev)
          case (Some(prev: NearestBefore[T]), Some(next: NearestAfter[T])) =>
            if (prev.distance < next.distance) {
              Some(prev)
            } else {
              Some(next)
            }
          case (Some(prev: SearchIntervalResult[T]), None) => Some(prev)
          case (None, Some(next)) => Some(next)
          case (None, None) => None

          // should not be possible but the type system doesn't agree
          case (Some(prev: NearestAfter[T]), Some(next: NearestAfter[T])) => throw new IllegalArgumentException()
      }
    }
  }
}
