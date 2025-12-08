@main def runAnalysis(): Unit =
  val bookings = DataUtils.loadBookings("Hotel_Dataset.csv")
  val analysisTasks = List(Question1(), Question2(), Question3())

  analysisTasks.foreach( question =>
    question.run(bookings)
    println()
  )