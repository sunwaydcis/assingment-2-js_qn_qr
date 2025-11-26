import scala.io.Source
import scala.util.{Try, Using}

case class HotelBooking(

                       originCountry: String,
                       noOfPeople: Int, hotelName: String,
                       bookingPrice: Double, discount: Double, profitMargin: Double
                       )

object HotelAnalysis:
  def main(args: Array[String]): Unit =

    val source = Source.fromResource("Hotel_Dataset.csv")
    val lines = source.getLines().drop(1)
    val rows = lines.map(_.split(",").map(_.trim))

    val selectRows = rows.map { cols =>
      val originCountry = cols(6)
      val hotelName = cols(16)
      val bookingPrice = cols(20)
      val discount = cols(21)
      val profitMargin = cols(23)
      val noOfPeople = cols(11)

      (originCountry, hotelName, bookingPrice, discount, profitMargin, noOfPeople)

    }
    val bookings = selectRows.map{(originCountry,hotelName, bookingPrice, discount, profitMargin, noOfPeople ) =>
      HotelBooking(
        originCountry = originCountry,
        hotelName = hotelName,
        bookingPrice = bookingPrice.toDouble,
        discount = discount.replace("%", "").toDouble,
        profitMargin = profitMargin.toDouble,
        noOfPeople = noOfPeople.toInt
      )
    }.toList

    //question1
    val countryMostBookings =
      bookings
        .groupBy(_.originCountry)
        .view.mapValues(_.size)
        .toMap
        .maxBy(_._2)
    println(s"Country with highest number of bookings is ${countryMostBookings._1} (${countryMostBookings._2})")

    // question 2.1
    val cheapestHotel = bookings.minBy(_.bookingPrice)


