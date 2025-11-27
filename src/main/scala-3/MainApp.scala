
@main def run() =
  val bookings = DataUtils.loadBookings()

  Question1().run(bookings)

  println()
  Question2().run(bookings)

  println()
  Question3().run(bookings)