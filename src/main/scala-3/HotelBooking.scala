import scala.io.Source
import scala.util.{Try, Using}

case class HotelBooking(

                       originCountry: String,
                       noOfPeople: Int, hotelName: String,
                       bookingPrice: Double, discount: Double, profitMargin: Double
                       )

object HotelBooking:
  def fromCsv(cols: Array[String]): HotelBooking=
    val originCountry = cols(6)
    val noOfPeople = cols(11).toInt
    val hotelName = cols(16)
    val bookingPrice = cols(20).toDouble
    val discount = cols(21).replace("%", "").toDouble
    val profitMargin = cols(23).toDouble

    HotelBooking(
      originCountry = originCountry,
      noOfPeople = noOfPeople,
      hotelName = hotelName,
      bookingPrice = bookingPrice,
      discount = discount,
      profitMargin = profitMargin
    )
    
  /*def main(args: Array[String]): Unit =

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
    println(s"Hotel with lowest booking price is ${cheapestHotel.hotelName} (${cheapestHotel.bookingPrice})")

    //cheapest price after discount
    val discountPrice =
      bookings.minBy(b=> b.bookingPrice * (1-b.discount / 100))

    val actualPrice = discountPrice.bookingPrice * (1 - discountPrice.discount /100)
    println(f"Hotel with lowest actual price after discount is ${discountPrice.hotelName} ($actualPrice%.2f)")

    // question 2.2
    val highestDiscountHotel = bookings.maxBy(_.discount)
    println(s"Hotel with highest discount is ${highestDiscountHotel.hotelName} (${highestDiscountHotel.discount}%)")

    //question 2.3
    val bestProfitHotel = bookings.maxBy(_.profitMargin)
    println(s"Hotel with best profit margin is ${bestProfitHotel.hotelName} (${bestProfitHotel.profitMargin})")

    //question3
    val mostProfitableHotel =
      bookings
        .groupBy(_.hotelName)
        .map { case (hotel, list) =>
          val totalProfitScore = list.map(b => b.noOfPeople * b.profitMargin).sum
          (hotel, totalProfitScore)
        }
        .maxBy(_._2)

    println(s"Most profitable hotel: ${mostProfitableHotel._1} (Score: ${mostProfitableHotel._2}%.2f)")*/



