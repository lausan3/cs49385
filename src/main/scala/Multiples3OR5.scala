object Multiples3Or5 {

  // If we list all the natural numbers below 10 that are multiples of 3 or 5, we 3, 5, 6 and 9.
  // The sum of these multiples is 23.
  // Find the sum of all the multiples of 3 or 5 below 1000.

  def go = {
    println(s"Sum for n = 10: ${getMultiples(10)}")
    println(s"Sum for n = 1000, ${getMultiples(1000)}")
    println(s"sum for n = 10 (using flatMap): ${getMultiplesFlatMap(10)}")
    println(s"sum for n = 1000, ${getMultiplesFlatMap(1000)}")
  }

  def getMultiples(high: Int): Int = (1 until high).foldLeft(0)( (acc: Int, x: Int) => if (x % 3 == 0 || x % 5 == 0) acc + x else acc )

  def getMultiplesFlatMap(high: Int): Int = (1 until high).flatMap( x => if (x % 3 == 0 || x % 5 == 0) List(x) else List() ).sum
}
