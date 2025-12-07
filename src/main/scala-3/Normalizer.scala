trait Normalizer:
  def normalizeLowBetter(value: Double, min: Double, max: Double): Double =
    if (max == min) 0.0 else 1 - (value - min) / (max - min)
  def normalizeHighBetter(value: Double, min: Double, max: Double): Double =
    if (max == min) 0.0 else (value - min) / (max - min)