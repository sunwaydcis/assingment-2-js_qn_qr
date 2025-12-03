
class Question3 extends Analysis, Normalizer:
  override def run(bookings: List[HotelBooking]): Unit =

    println("Question 3: Most Profitable Hotel")

    val profitMetrics: Map[String, (Int, Double, Int, Int, Double, Double)] =
      bookings
        .groupBy(_.hotelName)
        .map { case (hotel, list) =>
          val paxValues = list.map(_.noOfPeople)
          val profitValues = list.map(_.profitMargin)

          val avgPeople = paxValues.sum / list.size
          val avgProfit = profitValues.sum / list.size

          val minPeople = paxValues.min
          val maxPeople = paxValues.max

          val minProfit = profitValues.min
          val maxProfit = profitValues.max

          (hotel, (avgPeople, avgProfit, minPeople, maxPeople, minProfit, maxProfit))
        }


    // Normalize and calculate economy score
    val profitabilityScores = profitMetrics.map { case (hotel, (avgPeople, avgProfit, minPeople, maxPeople, minProfit, maxProfit)) =>
      // Normalize to 0-1 range
      val normPax = normalizeHighBetter(avgPeople, minPeople, maxPeople) // high val better
      val normProfit = normalizeHighBetter(avgProfit, minProfit, maxProfit) // high val better

      // Calculate economy score, higher score implies a more economical option for cust
      val profitabilityScore = (normPax + normProfit) / 2

      (hotel, profitabilityScore, avgPeople, avgProfit)
    }

    // Find the most economical hotel
    val mostProfitable = profitabilityScores.maxBy(tuple => tuple._2)
    println(f"Most Profitable Hotel: ${mostProfitable._1}")

    println(f"  Profitability Score: ${mostProfitable._2}%.4f")
    println(f"  Avg Booking People: ${mostProfitable._3}%.2f")
    println(f"  Avg Profit Margin: ${mostProfitable._4}%.2f")


//    val statsByHotel =
//      bookings
//        .groupBy(_.hotelName)
//        .map { case (hotel, list) =>
//          val totalProfitValues =
//            list.map { b =>
//              if b.rooms == 0 || b.noOfDays == 0 || b.profitMargin == 0 then
//                0.0
//              else
//                b.bookingPrice / b.rooms / b.noOfDays / b.profitMargin
//            }
//
//          val finalProfit = totalProfitValues.sum
//
//          (hotel, finalProfit)
//        }
//
//
//    val best = statsByHotel.maxBy(_._2)
//
//    println(f"Most profitable hotel is ${best._1} (total profit = ${best._2}%.2f)")
