
class Question1 extends Analysis:
  override def run(bookings: List[HotelBooking]): Unit =
    
    // Based on destination country
    val result =
      bookings
        .groupBy(_.destinationCountry)
        .view.mapValues(_.size)
        .maxBy(_._2)

    println(s"Country with highest bookings is ${result._1} (${result._2} bookings)")