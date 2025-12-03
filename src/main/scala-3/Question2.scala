class Question2 extends Analysis, Normalizer:

  override def run(bookings: List[HotelBooking]): Unit =

    println("Question 2: Most Economical Hotel")

    val hotelMetrics = bookings
      .groupBy(_.hotelName)
      .view
      .mapValues { hotelBookings =>
        val avgPrice = hotelBookings.map(_.bookingPrice).sum / hotelBookings.size
        val avgDiscount = hotelBookings.map(_.discount).sum / hotelBookings.size
        val avgProfitMargin = hotelBookings.map(_.profitMargin).sum / hotelBookings.size
        (avgPrice, avgDiscount, avgProfitMargin)
      }
      .toMap

    val prices = hotelMetrics.values.map(_._1)
    val discounts = hotelMetrics.values.map(_._2)
    val profitMargins = hotelMetrics.values.map(_._3)

    // Find min and max for each metric
    val (minPrice, maxPrice) = (prices.min, prices.max)
    val (minDiscount, maxDiscount) = (discounts.min, discounts.max)
    val (minProfitMargin, maxProfitMargin) = (profitMargins.min, profitMargins.max)

    // Normalize and calculate economy score
    val economyScores = hotelMetrics.map { case (hotel, (price, discount, profit)) =>
      // Normalize to 0-1 range
      val normPrice = normalizeLowBetter(price, minPrice, maxPrice) // low val is better so -1
      val normDiscount = normalizeHighBetter(discount, minDiscount, maxDiscount) // high val better
      val normProfit = normalizeLowBetter(profit, minProfitMargin, maxProfitMargin) // low better for cust

      // Calculate economy score, higher score implies a more economical option for cust
      val economyScore = normPrice + normDiscount + normProfit

      (hotel, economyScore, price, discount, profit)
    }

    // Find the most economical hotel
    val mostEconomical = economyScores.maxBy(tuple => tuple._2)
    println(f"Most Economical Hotel: ${mostEconomical._1}")





//    val prices = bookings.map(_.bookingPrice)
//    val discount = bookings.map(_.discount)
//    val profitMargin = bookings.map(_.profitMargin)
//
//    val minPrice = prices.min
//    val maxPrice = prices.max
//    val minDiscount = discount.min
//    val maxDiscount = discount.max
//    val minProfitMargin = profitMargin.min
//    val maxProfitMargin = profitMargin.max
//
//    def normalize(value: Double, min: Double, max: Double): Double =
//      (value - min) / (max - min) // formula to return normalized value from 0-1
////      if min == max then
//
//    val normalizedPrice = 1 - normalize(booking.bookingPrice, minPrice, maxPrice) // low val is better so -1
//    val normalizedDiscount = normalize(booking.discount, minDiscount, maxDiscount) // high val better
//    val normalizedProfitMargin = 1- normalize(booking.profitMargin, minProfitMargin, maxProfitMargin) // low better for cust
//






//
//    //2.1
//    val lowestPriceHotel = bookings.minBy(_.bookingPrice)
//    println(s"Hotel with lowest booking price is ${lowestPriceHotel.hotelName} (${lowestPriceHotel.bookingPrice})")
//
//    //2.2
//    val highestDiscountHotel = bookings.maxBy(_.discount)
//    println(s"Hotel with highest discount is ${highestDiscountHotel.hotelName} (${highestDiscountHotel.discount})")
//
//    //2.3
//    // more than one lowest profit margin hotel, should load all out
//    val lowerProfitHotel = bookings.minBy(_.profitMargin)
//
//    println(s"Hotel with lowest profit margin is ${lowerProfitHotel.hotelName} (${lowerProfitHotel.profitMargin})")
//
//    // cheapest price after discount
//    val discountPrice = bookings.minBy(b => b.bookingPrice * (1 - b.discount / 100))
//
//    val actualPrice = discountPrice.bookingPrice
//    println(f"Hotel with lowest actual price after discount is ${discountPrice.hotelName} ($actualPrice%.2f)")