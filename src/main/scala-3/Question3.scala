class Question3 extends Analysis:
  override def run(bookings: List[HotelBooking]): Unit =
    println("Question 3: Most Profitable Hotel:")

    // Group by unique hotel (name + country + city)
    val hotelStats =
      bookings
        .groupBy(b => (b.hotelName, b.destinationCountry, b.destinationCity))
        .map { case ((hotelName, country, city), hotelBookings) =>
          // Calculate the two required factors
          val totalVisitors = hotelBookings.map(_.noOfPeople).sum
          val avgProfitMargin = hotelBookings.map(_.profitMargin).sum / hotelBookings.size

          // Create a simple composite score (visitors × profit margin)
          val score = totalVisitors * avgProfitMargin

          (hotelName, country, city, totalVisitors, avgProfitMargin, score)
        }

    if (hotelStats.isEmpty) {
      println("No data available")
      return
    }


    // Find the hotel with the highest score
    val bestHotel = hotelStats.maxBy(_._6)
    val (hotelName, country, city, visitors, profit, score) = bestHotel

    println(s" Most Profitable Hotel: $hotelName")
    println(s" Location: $city, $country")
    println(f" Total Visitors: $visitors")
    println(f" Average Profit Margin: ${profit * 100}%.1f%%")
    println(f" Profit Score (visitors × profit margin): $score%.2f")