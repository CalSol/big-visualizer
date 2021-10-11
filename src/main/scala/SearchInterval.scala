package bigvis

import scala.collection.Searching.{Found, InsertionPoint, search}

object SearchInterval {
  sealed trait SearchIntervalResult {
    def index(): Int
  }
  case class ContainedIn(index: Int) extends SearchIntervalResult
  // The closest interval is before the search point, at the index (distance is always positive)
  case class NearestBefore(index: Int, distance: Double) extends SearchIntervalResult
  // The closest interval is after the search point, at the index (distance is always positive)
  case class NearestAfter(index: Int, distance: Double) extends SearchIntervalResult


  /** Given a data containing intervals, returns the index of the interval containing the search point,
   * or the index nearest the search point.
   */
  def apply(data: Seq[(Double, Double)], searchPoint: Double): Option[SearchIntervalResult] = {
    // Assumption: input is a sorted list, intervals do not overlap
    data.search((searchPoint, searchPoint))(Ordering.by(_._1)) match {  // sort by interval begin
      case Found(foundIndex) =>
        Some(ContainedIn(foundIndex))
      case InsertionPoint(insertionPoint) =>
        val prevOption = if (insertionPoint > 0) {
          val prevPoint = data(insertionPoint - 1)
          if (prevPoint._1 <= searchPoint && searchPoint <= prevPoint._2) {
            // prefer contained section
            Some(ContainedIn(insertionPoint - 1))
          } else {
            Some(NearestBefore(insertionPoint - 1, searchPoint - prevPoint._2))
          }
        } else {
          None
        }
        val nextOption = if (insertionPoint < data.length) {
          val nextPoint = data(insertionPoint)
          Some(NearestAfter(insertionPoint, nextPoint._1 - searchPoint))
        } else {
          None
        }

        (prevOption, nextOption) match {
          case (Some(prev: ContainedIn), _) => Some(prev)
          case (Some(prev: NearestBefore), Some(next: NearestAfter)) =>
            if (prev.distance < next.distance) {
              Some(prev)
            } else {
              Some(next)
            }
          case (Some(prev), None) => Some(prev)
          case (None, Some(next)) => Some(next)
          case (None, None) => None
      }
    }
  }
}
