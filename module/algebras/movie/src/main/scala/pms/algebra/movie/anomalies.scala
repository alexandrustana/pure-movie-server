package pms.algebra.movie

import busymachines.pureharm.anomaly._
import pms.core.AnomalyIDS

/**
  *
  * @author Lorand Szakacs, https://github.com/lorandszakacs
  * @since 09 May 2019
  *
  */
final case class MovieNotFoundAnomaly(mid: MovieID) extends NotFoundAnomaly(s"Movie with id '$mid' not found") {
  override val id: AnomalyID = AnomalyIDS.MovieNotFoundID

  override val parameters: Anomaly.Parameters = Anomaly.Parameters(
    "movieID" -> mid.toString
  )
}
