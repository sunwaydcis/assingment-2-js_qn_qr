class Question2 extends Analysis, Normalizer:

  override def run(bookings: List[HotelBooking]): Unit =

    println("Question 2: Most Economical Hotel")

    val hotelMetrics: Map[String, (Double, Double, Double, Double, Double, Double, Double, Double, Double)] =
      bookings
        .groupBy(_.hotelName)
        .map { case (hotel, list) =>
          val priceValues = list.map(b => b.bookingPrice / b.noOfDays / b.rooms)
          val discountValues = list.map(_.discount)
          val profitValues = list.map(_.profitMargin)

          val avgPrice = priceValues.sum / list.size
          val avgDiscount = discountValues.sum / list.size
          val avgProfit = profitValues.sum / list.size

          val minPrice = priceValues.min
          val maxPrice = priceValues.max

          val minDiscount = discountValues.min
          val maxDiscount = discountValues.max

          val minProfit = profitValues.min
          val maxProfit = profitValues.max

          (hotel, (avgPrice, avgDiscount, avgProfit,minPrice, maxPrice, minDiscount, maxDiscount, minProfit, maxProfit))
        }


    // Extract global min/max for same group of hotels (same hotel name)
//    val groupedMinPrice = hotelMetrics.values.map(_._4).min
//    val groupedMaxPrice = hotelMetrics.values.map(_._5).max
//
//    val groupedMinDiscount = hotelMetrics.values.map(_._6).min
//    val groupedMaxDiscount = hotelMetrics.values.map(_._7).max
//
//    val groupedMinProfit = hotelMetrics.values.map(_._8).min
//    val groupedMaxProfit = hotelMetrics.values.map(_._9).max

//    val prices = hotelMetrics.values.map(_._1)
//    val discounts = hotelMetrics.values.map(_._2)
//    val profitMargins = hotelMetrics.values.map(_._3)

    // Find min and max for each metric
//    val (minPrice, maxPrice) = (prices.min, prices.max)
//    val (minDiscount, maxDiscount) = (discounts.min, discounts.max)
//    val (minProfitMargin, maxProfitMargin) = (profitMargins.min, profitMargins.max)

    // Normalize and calculate economy score
    val economyScores = hotelMetrics.map { case (hotel, (avgPrice, avgDiscount, avgProfit, minPrice, maxPrice, minDiscount, maxDiscount, minProfit, maxProfit)) =>
      // Normalize to 0-1 range
      val normPrice = normalizeLowBetter(avgPrice, minPrice, maxPrice) // low val is better so -1
      val normDiscount = normalizeHighBetter(avgDiscount, minDiscount, maxDiscount) // high val better
      val normProfit = normalizeLowBetter(avgProfit, minProfit, maxProfit) // low better for cust

      // Calculate economy score, higher score implies a more economical option for cust
      val economyScore = (normPrice + normDiscount + normProfit) / 3

      (hotel, economyScore, avgPrice, avgDiscount, avgProfit)
    }

    // Find the most economical hotel
    val mostEconomical = economyScores.maxBy(tuple => tuple._2)
    println(f"Most Economical Hotel: ${mostEconomical._1}")

    println(f"  Economy Score: ${mostEconomical._2}%.4f")
    println(f"  Avg Booking Price: ${mostEconomical._3}%.2f")
    println(f"  Avg Discount: ${mostEconomical._4}%.2f%%")
    println(f"  Avg Profit Margin: ${mostEconomical._5}%.2f")







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
