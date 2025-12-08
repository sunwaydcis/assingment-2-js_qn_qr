import scala.io.Source
import scala.util.{Try, Using}

case class HotelBooking(

                       originCountry: String, destinationCountry: String,
                       destinationCity: String, noOfPeople: Int, noOfDays: Int, hotelName: String,
                       rooms: Int,bookingPrice: Double, discount: Double, profitMargin: Double
                       )

object HotelBooking:
  def fromCsv(cols: Array[String]): HotelBooking=
    val originCountry = cols(6)
    val destinationCountry = cols(9)
    val destinationCity = cols(10)
    val noOfPeople = cols(11).toInt
    val noOfDays = cols(13).toInt
    val rooms = cols(15).toInt
    val hotelName = cols(16)
    val bookingPrice = cols(20).toDouble
    val discount = cols(21).replace("%", "").toDouble
    val profitMargin = cols(23).toDouble

    HotelBooking(
      originCountry = originCountry,
      destinationCountry = destinationCountry,
      destinationCity = destinationCity,
      noOfPeople = noOfPeople,
      noOfDays = noOfDays,
      rooms = rooms,
      hotelName = hotelName,
      bookingPrice = bookingPrice,
      discount = discount,
      profitMargin = profitMargin
    )