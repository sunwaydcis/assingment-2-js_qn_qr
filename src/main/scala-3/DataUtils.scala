import scala.io.Source
import scala.util.Using

object DataUtils:
  def loadBookings(): List[HotelBooking] =
    Using.resource(Source.fromResource("Hotel_Dataset.csv")) { source =>
      source 
        .getLines()
        .drop(1)
        .map(_.split(",").map(_.trim))
        .map(HotelBooking.fromCsv)
        .toList
    }