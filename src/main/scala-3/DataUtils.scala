import scala.io.Source
import scala.util.Using

object DataUtils:
  // Improve code reusability
  def loadBookings(dataset: String): List[HotelBooking] =
    Using.resource(Source.fromResource(dataset)) { source =>
      source 
        .getLines()
        .drop(1)
        .map(_.split(",").map(_.trim))
        .map(HotelBooking.fromCsv)
        .toList
    }