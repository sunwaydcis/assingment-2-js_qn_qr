@main def run() =
  val bookings = DataUtils.loadBookings("Hotel_Dataset.csv")
  val analysisTasks = List(Question1(), Question2(), Question3())

  analysisTasks.foreach( question =>
    question.run(bookings)
    println()
  )

//  Question1().run(bookings)
//
//  println()
//  Question2().run(bookings)
//
//  println()
//  Question3().run(bookings)