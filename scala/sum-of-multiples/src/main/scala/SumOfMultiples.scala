object SumOfMultiples {
  def sum(factors: Set[Int], limit: Int): Int = {

    val factorList = factors.toList
    val min = factorList.foldLeft(factors.head)((a, b) => if (a < b) a else b)
    val divisibleFor = (value: Int, factors: List[Int]) => factors.foldRight(true)((a, b) => (value % a == 0) && b)

    (min to limit by min).toList.filter(divisibleFor(_, factorList)).sum
  }

}

