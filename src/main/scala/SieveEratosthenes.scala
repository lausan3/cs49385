object SieveEratosthenes {

  def go = {
    val n = 30
    val r = allPrimes(n)
    println(s"All primes smaller than ${n}: ${r}")
  }

  def allPrimes(n: Int): List[Int] = {
    def p(candidates: List[Int], results: List[Int]): List[Int] = {
      if (candidates.length == 0) results.reverse
      else  {
        val new_prime = candidates.head
        p(candidates.filter(x => x % new_prime != 0), new_prime :: results)
      }
    }
    p((2 to n).toList, List.empty[Int])
  }

}
