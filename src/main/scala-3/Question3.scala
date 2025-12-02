
class Question3 extends Analysis:
  override def run(bookings: List[HotelBooking]): Unit =
    
    //highest profit is calculate based on no of people or no of day or both 

    // based on days and rooms
    val hotelProfit =
    bookings
      .groupBy(_.hotelName)
      .map { case (hotel, list) =>
        val totalProfit = list.map(b => b.bookingPrice * b.rooms * b.noOfDays * b.profitMargin).sum
        (hotel, totalProfit)
      }

    val highest = hotelProfit.maxBy(_._2)


    println(f"Most profitable hotel: ${highest._1} (${highest._2}%.2f)")
