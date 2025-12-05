class Question2 extends Analysis, Normalizer:

  override def run(bookings: List[HotelBooking]): Unit =

    println("Question 2: Most Economical Hotel:")
    val statsHotel =
      bookings
        .groupBy(b => (b.hotelName, b.destinationCountry, b.destinationCity))
        .map { case (key, list) =>
          val avgPrice = list.map(_.bookingPrice).sum / list.size
          val avgDiscount = list.map(_.discount).sum / list.size
          val avgProfit = list.map(_.profitMargin).sum / list.size
          (key, (avgPrice, avgDiscount, avgProfit))
        }


    val prices = statsHotel.values.map(_._1).toList
    val discounts = statsHotel.values.map(_._2).toList
    val profits = statsHotel.values.map(_._3).toList

    val minPrice = prices.min
    val maxPrice = prices.max
    val minDiscount = discounts.min
    val maxDiscount = discounts.max
    val minProfit = profits.min
    val maxProfit = profits.max

    // normalization
    def normalizeLowBetter(value: Double, minV: Double, maxV: Double): Double =
      if (maxV == minV) 0.0 else (value - minV) / (maxV - minV) // 0 = best (min)


    // for discount, higher is better
    def normalizeHighBetter(value: Double, minV: Double, maxV: Double): Double =
      if (maxV == minV) 0.0 else 1.0 - ((value - minV) / (maxV - minV)) // 0 = best (max discount)

    type HotelKey = (String, String, String) // (hotelName, destinationCountry, destinationCity)

    // compute composite score — note the key type is HotelKey, not String
    val scoreByHotel: Map[HotelKey, Double] =
      statsHotel.map { case (hotelKey, (avgPrice, avgDiscount, avgProfit)) =>
        val np = normalizeLowBetter(avgPrice, minPrice, maxPrice)
        val nd = normalizeHighBetter(avgDiscount, minDiscount, maxDiscount)
        val npr = normalizeLowBetter(avgProfit, minProfit, maxProfit)
        val composite = np + nd + npr
        (hotelKey, composite)
      }

    val best = scoreByHotel.minBy(_._2)
    val bestHotelKey = best._1
    val bestScore = best._2

    // destructure the tuple key for pretty printing
    val (bestHotelName, bestDestCountry, bestDestCity) = bestHotelKey
    val (avgP, avgD, avgPr) = statsHotel(bestHotelKey)

    println(f" Most Economical Hotel is  $bestHotelName, Location is  $bestDestCity, $bestDestCountry ")
    println(f" Avg price: ${avgP}%.2f, Avg discount: ${avgD}%.2f%%, Avg profit margin: ${avgPr}%.2f")









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
