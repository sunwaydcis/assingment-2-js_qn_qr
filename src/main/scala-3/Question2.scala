class Question2 extends Question:

  override def run(bookings: List[HotelBooking]): Unit =
    println("Question 2: Most Economical Hotel:")

    // This will calculate display all hotels with same hotel name, but in diff city & country separately (as diff transaction records)
    val statsHotel = computeHotelStats(bookings) { list =>
      val avgPrice = list.map(_.bookingPrice).sum / list.size
      val avgDiscount = list.map(_.discount).sum / list.size
      val avgProfit = list.map(_.profitMargin).sum / list.size
      (avgPrice, avgDiscount, avgProfit)
    }

    // After obtaining average for each transaction record (i.e. same hotelName, but diff locations,
    // group them by same hotelName to calculate min-max values (shown in next step)
    val groupedByName = groupByHotelName(statsHotel)

    // Calculate min-max for each hotel chain
    val minMaxByHotel = calculateMinMax3(
      groupedByName,
      { case (price, _, _) => price },
      { case (_, discount, _) => discount },
      { case (_, _, profit) => profit }
    )

    // Calculate economy score of each hotel (each hotel located in diff city & location are treated as separate)
    val economyScores = statsHotel.map {
      case ((hotelName, country, city), (avgPrice, avgDiscount, avgProfit)) =>
        val (minP, maxP, minD, maxD, minPf, maxPf) = minMaxByHotel(hotelName)

        val normPrice = normalizeLowBetter(avgPrice, minP, maxP)
        val normDiscount = normalizeHighBetter(avgDiscount, minD, maxD)
        val normProfit = normalizeLowBetter(avgProfit, minPf, maxPf)

        val score = (normPrice + normDiscount + normProfit) / 3.0

        (hotelName, country, city, score, avgPrice, avgDiscount, avgProfit)
    }

    // Find most economical hotel based on score
    val (hotelName, country, city, score, avgPrice, avgDiscount, avgProfit) =
      economyScores.maxBy(_._4)

    println(f" Most Economical Hotel is  $hotelName, Location is  $city, $country ")
    println(f" Avg price: ${avgPrice}%.2f, Avg discount: ${avgDiscount}%.2f%%, Avg profit margin: ${avgProfit * 100}%.2f%%")