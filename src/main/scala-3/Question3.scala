class Question3 extends Question:
  override def run(bookings: List[HotelBooking]): Unit =
    println("Question 3: Most Profitable Hotel:")

    // Calculate stats for each hotel branch
    val statsHotel = computeHotelStats(bookings) { list =>
      val totalVisitors = list.map(_.noOfPeople).sum
      val avgProfitMargin = list.map(_.profitMargin).sum / list.size
      (totalVisitors, avgProfitMargin)
    }

    // Group by hotel name
    val groupedByName = groupByHotelName(statsHotel)

    // Calculate min-max for each hotel chain
    val minMaxByHotel = calculateMinMax2(
      groupedByName,
      { case (visitors, _) => visitors },
      { case (_, profit) => profit }
    )

    // Calculate profit scores
    val profitScores = statsHotel.map {
      case ((hotelName, country, city), (totalVisitors, avgProfitMargin)) =>
        val (minVisitors, maxVisitors, minProfit, maxProfit) = minMaxByHotel(hotelName)

        val normVisitors = normalizeHighBetter(totalVisitors, minVisitors, maxVisitors)
        val normProfit = normalizeHighBetter(avgProfitMargin, minProfit, maxProfit)

        val score = (normVisitors + normProfit) / 2.0

        (hotelName, country, city, score, totalVisitors, avgProfitMargin, normVisitors, normProfit)
    }

    // Find most profitable hotel based on score
    val (hotelName, country, city, score, totalVisitors, avgProfitMargin, normVisitors, normProfit) =
      profitScores.maxBy(_._4)

    println(f" Most Profitable Hotel is  $hotelName, Located at $city, $country")
    println(f" Total visitors: $totalVisitors%d, Avg Profit Margin: ${avgProfitMargin * 100}%.2f%%")