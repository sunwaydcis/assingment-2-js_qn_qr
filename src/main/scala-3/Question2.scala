class Question2 extends Analysis, Normalizer:

  override def run(bookings: List[HotelBooking]): Unit =

    println("Question 2: Most Economical Hotel:")

    // This will calculate display all hotels with same hotel name, but in diff city & country separately (as diff transaction records)
    val statsHotel =
      bookings
        .groupBy(b => (b.hotelName, b.destinationCountry, b.destinationCity))
        .map { case (key, list) =>
          val avgPrice = list.map(_.bookingPrice).sum / list.size
          val avgDiscount = list.map(_.discount).sum / list.size
          val avgProfit = list.map(_.profitMargin).sum / list.size
          (key, (avgPrice, avgDiscount, avgProfit))
        }
//    println(s"All: ${statsHotel}")

    // After obtaining average for each transaction record (i.e. same hotelName, but diff locations,
    // group them by same hotelName to calculate min-max values (shown in next step)
    val groupedByName =
      statsHotel
        .groupBy { case ((hotelName, _, _), _) => hotelName }

    // Min-max calculations
    val minMaxByHotel = groupedByName.map { case (hotelName, records) =>

      val prices = records.map { case (_, (price, _, _)) => price }
      val discounts = records.map { case (_, (_, discount, _)) => discount }
      val profits = records.map { case (_, (_, _, profit)) => profit }

      val minPrice = prices.min
      val maxPrice = prices.max

      val minDiscount = discounts.min
      val maxDiscount = discounts.max

      val minProfit = profits.min
      val maxProfit = profits.max

      hotelName -> (minPrice, maxPrice, minDiscount, maxDiscount, minProfit, maxProfit)
    }

//    println(s"Min max: $minMaxByHotel")

    // Calculate economy score of each hotel (each hotel located in diff city & location are treated as separate)
    val economyScores = statsHotel.map { case ((hotelName, country, city), (avgPrice, avgDiscount, avgProfit)) =>

      val (minP, maxP, minD, maxD, minPf, maxPf) = minMaxByHotel(hotelName)

      val normPrice = normalizeLowBetter(avgPrice, minP, maxP)
      val normDiscount = normalizeHighBetter(avgDiscount, minD, maxD)
      val normProfit = normalizeLowBetter(avgProfit, minPf, maxPf)

      val score = (normPrice + normDiscount + normProfit) / 3.0

      (hotelName, country, city, score, avgPrice, avgDiscount, avgProfit)
    }

    // Most economical hotel based on score
    val best = economyScores.maxBy(_._4)

    // Extract final results
    val (hotelName, country, city, score, avgPrice, avgDiscount, avgProfit) = best

    println(f" Most Economical Hotel is  $hotelName, Location is  $city, $country ")
    println(f" Avg price: ${avgPrice}%.2f, Avg discount: ${avgDiscount}%.2f%%, Avg profit margin: ${avgProfit}%.2f")