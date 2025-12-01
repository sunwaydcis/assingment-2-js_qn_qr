
class Question3 extends Analysis:
  override def run(bookings: List[HotelBooking]): Unit =
    
    //highest profit is calculate based on no of people or no of day or both 
    
    val HotelProfit =
      bookings
        .groupBy(_.hotelName)
        .map { case (hotel, list) =>
          val totalProfit = list.map(b => b.profitMargin * b.noOfPeople).sum
          (hotel, totalProfit)
        }

    val highestProfit = HotelProfit.maxBy(_._2)

    println(f"Most profitable hotel: ${highestProfit._1} (${highestProfit._2}%.2f)")
