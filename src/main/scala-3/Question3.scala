
class Question3 extends Analysis:
  override def run(bookings: List[HotelBooking]): Unit =

    val statsByHotel =
      bookings
        .groupBy( b => (b.hotelName, b.destinationCountry, b.destinationCity))
        .map{ case (key, list) =>
          val avgPrice = list.map(_.bookingPrice).sum / list.size
          val avgProfit = list.map(_.profitMargin).sum / list.size
          val avgPeople = list.map(_.noOfPeople).sum / list.size
          (key, (avgPrice, avgProfit, avgPeople))
        }


    val prices = statsByHotel.values.map(_._1).toList
    val profits = statsByHotel.values.map(_._2).toList
    val peoples = statsByHotel.values.map(_._3).toList

    val minPrice = prices.min
    val maxPrice = prices.max
    val minProfit = profits.min
    val maxProfit = profits.max
    val minPeople = peoples.min
    val maxPeople = peoples.max

    def normalizeLowBetter(v: Double, minV: Double, maxV: Double): Double =
      if (maxV == minV) 0.0 else (v - minV) / (maxV - minV)

    def normalizeHighBetter(v: Double, minV: Double, maxV: Double): Double =
      if (maxV == minV) 0.0 else 1.0 - ((v - minV) / (maxV - minV))

    // compute
    type HotelKey = (String, String, String)

    val scoreByHotel: Map[HotelKey, Double] =
      statsByHotel.map{ case (key, (avgPrice, avgProfit, avgPeople)) =>
        val np = normalizeHighBetter(avgPrice, minPrice, maxPrice)
        val npr = normalizeHighBetter(avgProfit, minProfit, maxProfit)
        val nv = normalizeHighBetter(avgPeople, minPeople, maxPeople)
        val composite = np + npr + nv
        (key, composite)
      }

    val best = scoreByHotel.minBy(_._2)
    val (hotelName, destCountry, destCity) = best._1
    val bestScore = best._2

    val (avgP, avgPr, avgPeo) = statsByHotel(best._1)

    println(f"Best profitable hotel: $hotelName, $destCity, $destCountry (score: $bestScore%.4f)")
    println(f"Avg price: $avgP%.2f, Avg profit margin: $avgPr%.2f, Avg visitors: $avgPeo%.2f")

