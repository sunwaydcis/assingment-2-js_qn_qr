class Question3 extends Analysis:
  override def run(bookings: List[HotelBooking]): Unit =
    println("Question 3: Most Profitable Hotel:")

    // Group by unique hotel (name + country + city) and calculate metrics
    val hotelStats =
      bookings
        .groupBy(b => (b.hotelName, b.destinationCountry, b.destinationCity))
        .map { case ((hotelName, country, city), hotelBookings) =>
          val totalVisitors = hotelBookings.map(_.noOfPeople).sum
          val avgProfitMargin = hotelBookings.map(_.profitMargin).sum / hotelBookings.size

          (hotelName, country, city, totalVisitors, avgProfitMargin)
        }.toList

    if (hotelStats.isEmpty) {
      println("No data available")
      return
    }

    // Find min and max values for normalization
    val allVisitors = hotelStats.map(_._4)
    val allMargins = hotelStats.map(_._5)

    val minVisitors = allVisitors.min
    val maxVisitors = allVisitors.max
    val minMargin = allMargins.min
    val maxMargin = allMargins.max

    // Calculate normalized scores for each hotel
    val hotelsWithScores = hotelStats.map {
      case (hotelName, country, city, visitors, margin) =>
        // Normalize visitors: (visitors - min) / (max - min)
        val visitorScore =
          if (maxVisitors > minVisitors)
            (visitors - minVisitors).toDouble / (maxVisitors - minVisitors)
          else 0.5

        // Normalize profit margin: (margin - min) / (max - min)
        val marginScore =
          if (maxMargin > minMargin)
            (margin - minMargin) / (maxMargin - minMargin)
          else 0.5

        // Combined score: 50% visitors + 50% profit margin
        val combinedScore = (visitorScore + marginScore) / 2

        (hotelName, country, city, visitors, margin, visitorScore, marginScore, combinedScore)
    }

    // Find the hotel with highest combined score
    val bestHotel = hotelsWithScores.maxBy(_._8)
    val (hotelName, country, city, visitors, margin, visitorScore, marginScore, combinedScore) = bestHotel

    // Print results
    println(s" Most Profitable Hotel: $hotelName")
    println(s" Location: $city, $country")
    println(f" Total Visitors: $visitors")
    println(f" Average Profit Margin: ${margin * 100}%.1f%%")
    println(f" Normalized Visitor Score: $visitorScore%.4f")
    println(f" Normalized Profit Margin Score: $marginScore%.4f")
    println(f" Combined Score: $combinedScore%.4f")