
class Question1 extends Analysis:
  override def run(bookings: List[HotelBooking]): Unit =
    
    //not confirmed is from origin country or the destination country
    
    val result =
      bookings
        .groupBy(_.originCountry)
        .view.mapValues(_.size)
        .maxBy(_._2)
        
    val result2 =
      bookings
        .groupBy(_.destinationCountry)
        .view.mapValues(_.size)
        .maxBy(_._2)

    println(s"Country with highest bookings is ${result._1} (${result._2} bookings)")
    println(s"Country with highest bookings is ${result2._1} (${result2._2} bookings)")