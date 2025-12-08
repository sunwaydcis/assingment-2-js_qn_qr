class Question3 extends Analysis, Normalizer:
  override def run(bookings: List[HotelBooking]): Unit =
    println("Question 3: Most Profitable Hotel:")

    // Group by unique hotel (name + country + city) and calculate metrics
    val statsHotel =
      bookings
        .groupBy(b => (b.hotelName, b.destinationCountry, b.destinationCity))
        .map { case (key, list) =>
          val totalVisitors = list.map(_.noOfPeople).sum
          val avgProfitMargin = list.map(_.profitMargin).sum / list.size

          (key, (totalVisitors, avgProfitMargin))
        }

//    println(s"All: ${statsHotel}")

    // After obtaining average for each transaction record (i.e. same hotelName, but diff locations,
    // group them by same hotelName to calculate min-max values (shown in next step)
    val groupedByName =
      statsHotel
        .groupBy { case ((hotelName, _, _), _) => hotelName }

//    println(groupedByName)

    // Min-max calculations
    val minMaxByHotel = groupedByName.map { case (hotelName, records) =>

      val visitorsList = records.map { case (_, (totalVisitors, _)) => totalVisitors }
      val profitMargins = records.map { case (_, (_, avgProfitMargin)) => avgProfitMargin }

      val minVisitors = visitorsList.min
      val maxVisitors = visitorsList.max

      val minProfit = profitMargins.min
      val maxProfit = profitMargins.max

      hotelName -> (minVisitors, maxVisitors, minProfit, maxProfit)
    }

//    minMaxByHotel.foreach { case (hotel, (minV, maxV, minP, maxP)) =>
//      println(s"$hotel -> (minV=$minV, maxV=$maxV, minProfit=$minP, maxProfit=$maxP)")
//    }
//    println(s"Min max: $minMaxByHotel")
//    println(s"Min/Max check: ${minMaxByHotel.take(5)}") // Verify it has 4 values

    // Calculate score (treat each branch as separate)
    val profitScores = statsHotel.map { case ((hotelName, country, city), (totalVisitors, avgProfitMargin)) =>

      val (minVisitors, maxVisitors, minProfit, maxProfit) = minMaxByHotel(hotelName)

      // Define your normalizations
      val normVisitors = normalizeHighBetter(totalVisitors, minVisitors, maxVisitors)
      val normProfit = normalizeHighBetter(avgProfitMargin, minProfit, maxProfit)

      // Only two metrics now → average the two
      val score = (normVisitors + normProfit) / 2.0

      (hotelName, country, city, score, totalVisitors, avgProfitMargin, normVisitors, normProfit)
    }

    val best = profitScores.maxBy(_._4)

    val (hotelName, country, city, score, totalVisitors, avgProfitMargin, normVisitors, normProfit) = best

//    println(s"Score: $score")
//    println(s"Normalized Visitors: $normVisitors")
//    println(s"Normalized Profit: $normProfit")

    println(f" Most Profitable Hotel is  $hotelName, Located at $city, $country")
    println(f" Total visitors: $totalVisitors%d, Avg Profit Margin: ${avgProfitMargin}%.2f")
//    println(f" $score")



//    if (hotelStats.isEmpty) {
//      println("No data available")
//      return
//    }
//
//    // Find min and max values for normalization
//    val allVisitors = hotelStats.map(_._4)
//    val allMargins = hotelStats.map(_._5)
//
//    val minVisitors = allVisitors.min
//    val maxVisitors = allVisitors.max
//    val minMargin = allMargins.min
//    val maxMargin = allMargins.max
//
//    // Calculate normalized scores for each hotel
//    val hotelsWithScores = hotelStats.map {
//      case (hotelName, country, city, visitors, margin) =>
//        // Normalize visitors: (visitors - min) / (max - min)
//        val visitorScore =
//          if (maxVisitors > minVisitors)
//            (visitors - minVisitors).toDouble / (maxVisitors - minVisitors)
//          else 0.5
//
//        // Normalize profit margin: (margin - min) / (max - min)
//        val marginScore =
//          if (maxMargin > minMargin)
//            (margin - minMargin) / (maxMargin - minMargin)
//          else 0.5
//
//        // Combined score: 50% visitors + 50% profit margin
//        val combinedScore = (visitorScore + marginScore) / 2
//
//        (hotelName, country, city, visitors, margin, visitorScore, marginScore, combinedScore)
//    }
//
//    // Find the hotel with highest combined score
//    val bestHotel = hotelsWithScores.maxBy(_._8)
//    val (hotelName, country, city, visitors, margin, visitorScore, marginScore, combinedScore) = bestHotel

//    // Print results
//    println(s" Most Profitable Hotel: $hotelName")
//    println(s" Location: $city, $country")
//    println(f" Total Visitors: $visitors")
//    println(f" Average Profit Margin: ${margin * 100}%.1f%%")
//    println(f" Normalized Visitor Score: $visitorScore%.4f")
//    println(f" Normalized Profit Margin Score: $marginScore%.4f")
//    println(f" Combined Score: $combinedScore%.4f")