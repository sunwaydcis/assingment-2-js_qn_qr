
class Question2 extends Analysis:

  override def run(bookings: List[HotelBooking]): Unit =
    //2.1
    val lowestPriceHotel = bookings.minBy(_.bookingPrice)
    println(s"Hotel with lowest booking price is ${lowestPriceHotel.hotelName} (${lowestPriceHotel.bookingPrice})")

    // cheapest price after discount
    val discountPrice = bookings.minBy(b => b.bookingPrice * (1 - b.discount / 100))

    val actualPrice = discountPrice.bookingPrice * (1 - discountPrice.discount / 100)
    println(f"Hotel with lowest actual price after discount is ${discountPrice.hotelName} ($actualPrice%.2f)")

    //2.2
    val highestDiscountHotel = bookings.maxBy(_.discount)
    println(s"Hotel with highest discount is ${highestDiscountHotel.hotelName} (${highestDiscountHotel.discount}%)")

    //2.3
    val bestProfitHotel = bookings.maxBy(_.profitMargin)
    println(s"Hotel with best profit margin is ${bestProfitHotel.hotelName} (${bestProfitHotel.profitMargin})")
