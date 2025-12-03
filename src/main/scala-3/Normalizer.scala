trait Normalizer:
  def normalizeLowBetter(value: Double, min: Double, max: Double): Double =
    1 - (value - min) / (max - min)
  def normalizeHighBetter(value: Double, min: Double, max: Double): Double =
    (value - min) / (max - min)