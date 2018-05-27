object Leap {

  def leapYear(year: Int): Boolean = year match {
    case x if x % 4 == 0 => x % 100 == 0 || x % 400 == 0
    case _ => false
  }

}