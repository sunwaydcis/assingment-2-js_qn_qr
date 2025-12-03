
class Question3 extends Analysis:
  override def run(bookings: List[HotelBooking]): Unit =

    val statsByHotel =
      bookings
        .groupBy(_.hotelName)
        .map { case (hotel, list) =>
          val totalProfitValues =
            list.map { b =>
              if b.rooms == 0 || b.noOfDays == 0 || b.profitMargin == 0 then
                0.0
              else
                b.bookingPrice / b.rooms / b.noOfDays / b.profitMargin
            }

          val finalProfit = totalProfitValues.sum

          (hotel, finalProfit)
        }


    val best = statsByHotel.maxBy(_._2)

    println(f"Most profitable hotel is ${best._1} (total profit = ${best._2}%.2f)")
