object SpaceAge {

  def onEarth(seconds: Double): Double = {
    calculateYearComparedToHeart(seconds,1)

  }

  def onMercury(seconds: Double): Double = {
    calculateYearComparedToHeart(seconds,0.2408467)
  }


  def onVenus(seconds: Double) = {
    calculateYearComparedToHeart(seconds,0.61519726)
  }


  def onMars(seconds: Double) = {
    calculateYearComparedToHeart(seconds,1.8808158)
  }


  def onJupiter(seconds: Double) = {
    calculateYearComparedToHeart(seconds,11.862615)
  }


  def onSaturn(seconds: Double) = {
    calculateYearComparedToHeart(seconds,29.447498)
  }


  def onUranus(seconds: Double) = {
    calculateYearComparedToHeart(seconds,84.016846)
  }


  def onNeptune(seconds: Double) = {
    calculateYearComparedToHeart(seconds,164.79132)
  }

  private def calculateYearComparedToHeart(seconds: Double,constant: Double): Double = {
    val days = 365.25
    val secondsInAYear = days * 24 * 60 * 60
    BigDecimal(seconds / secondsInAYear / constant).setScale(2, BigDecimal.RoundingMode.HALF_UP).toDouble
  }

}
