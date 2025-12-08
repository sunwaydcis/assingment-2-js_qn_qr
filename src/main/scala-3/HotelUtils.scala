abstract class Question extends Analysis, Normalizer, HotelGrouping, MinMaxCalculator 

trait Normalizer:
  def normalizeLowBetter(value: Double, min: Double, max: Double): Double =
    if (max == min) 0.0 else 1 - (value - min) / (max - min)
  def normalizeHighBetter(value: Double, min: Double, max: Double): Double =
    if (max == min) 0.0 else (value - min) / (max - min)

trait HotelGrouping:
  def computeHotelStats[T](bookings: List[HotelBooking])
                          (metricFn: List[HotelBooking] => T): Map[(String, String, String), T] = {
    bookings
      .groupBy(b => (b.hotelName, b.destinationCountry, b.destinationCity))
      .map { case (key, list) => key -> metricFn(list) }
  }

  def groupByHotelName[T](stats: Map[(String, String, String), T]): Map[String, Map[(String, String, String), T]] = {
    stats.groupBy { case ((hotelName, _, _), _) => hotelName }
  }

trait MinMaxCalculator:
  // Generic min-max calculation for tuples of any size
  def calculateMinMax2[T](
                           groupedData: Map[String, Map[(String, String, String), T]],
                           extractor1: T => Double,
                           extractor2: T => Double
                         ): Map[String, (Double, Double, Double, Double)] = {
    groupedData.map { case (hotelName, records) =>
      val values1 = records.values.map(extractor1)
      val values2 = records.values.map(extractor2)

      hotelName -> (values1.min, values1.max, values2.min, values2.max)
    }
  }

  def calculateMinMax3[T](
                           groupedData: Map[String, Map[(String, String, String), T]],
                           extractor1: T => Double,
                           extractor2: T => Double,
                           extractor3: T => Double
                         ): Map[String, (Double, Double, Double, Double, Double, Double)] = {
    groupedData.map { case (hotelName, records) =>
      val values1 = records.values.map(extractor1)
      val values2 = records.values.map(extractor2)
      val values3 = records.values.map(extractor3)

      hotelName -> (values1.min, values1.max, values2.min, values2.max, values3.min, values3.max)
    }
  }