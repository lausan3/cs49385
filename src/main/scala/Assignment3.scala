import scala.util.Random
import SieveEratosthenes.allPrimes
import ReverseLinkedList.{reverseListFunctional, viewList}
import Multiples3Or5.getMultiples
import BernoulliTrials.{RESULTS_2D, conductAllTrials}

object Assignment3 {

  // Task 1. lambda expression for PI approximation
  // i.
  val pi_approximation: Seq[(Double,Double)] => Double = points => {
    val pointsInsideCircle = points.count({ case (x, y) => x * x + y * y <= 1 })
    4.0 * pointsInsideCircle / points.size
  }

  // ii.
  val values: Random => Int => Seq[(Double,Double)] = r => numTrials =>
      Seq.fill(numTrials)((r.nextDouble(), r.nextDouble()))

  // 2. approximate PI using a stream of values, wrapped in an option
  // i.
  val streamed_values: Random => Stream[(Double,Double)] = r => {
    def f(x: (Double, Double)): Stream[(Double, Double)] = x #:: f((r.nextDouble(), r.nextDouble()))
    f((r.nextDouble(), r.nextDouble()))
  }

  // ii.
  def pi_approximation_option(v: Seq[(Double,Double)])(n: Int): Option[Double] = {
    val takenStream = v.take(n)

    if (takenStream.length < n) {
      return None
    }

    val pointsInsideCircle = takenStream.foldLeft(Seq.empty[(Double, Double)])( (acc, point) =>
      try {
        point match {
          case (x, y) => if (x * x + y * y <= 1) point +: acc else acc
          case _ => acc
        }
      } catch {
        case _: Throwable => return None
      }
    )
    Some(4.0 * pointsInsideCircle.length / n)
  }
  // 3. Lifting assignment 2 items
  def lift[A,B](f: A => B): Option[A] => Option[B] = {
    case Some(a) => Some(f(a))
    case None => None
  }

  // i.
  // Sieve of Eratosthenes
  val allPrimesOption: Option[Int] => Option[List[Int]] = lift(allPrimes)

  // Reverse Linked List
  val reverseListOption: Option[ListNode] => Option[ListNode] = lift(reverseListFunctional)

  // Multiples of 3 or 5
  val getMultiplesOption: Option[Int] => Option[Int] = lift(getMultiples)

  // ii.
  // Bernoulli trials
  val conductAllTrialsOption: (Option[Random],Option[Int],Option[Int],Option[Int]) => Option[RESULTS_2D] = {
    case (Some(a), Some(b), Some(c), Some(d)) => Some(conductAllTrials(a, b, c, d))
    case _ => None
  }

  // 4. Corecursion applied to recurrence relations
  // i. factorials
  val factorials: Stream[Int] = { def g(n: Int, f0: Int): Stream[Int] = f0 #:: g(n+1, n*f0); g(1, 1) }
  // ii. Fibonacci numbers: t_1 = 1, t_2 = 1, t_n = t_n-1 + t_n-2
  val fibs: Stream[Int] = { def g(f0: Int, f1: Int): Stream[Int] = f0 #:: g(f1, f0+f1); g(1, 1) }
  // iii. a third order recurrence relation: t_1 = 2, t_2 = 3, t_3 = 5, t_n = 2 * t_n-1 + 7 * t_n-2 + 9 * t_n-3
  val t: Stream[Int] = { def g(t1: Int, t2: Int, t3: Int): Stream[Int] = t1 #:: g(t2, t3, 2 * t3 + 7 * t2 + 9 * t1); g(2, 3, 5) }

  // iv.
  // define using corecursion an infinite stream of positive integers starting with 2, i.e. [2,3,4,5,6,7,...]
  val naturals: Stream[Int] = { def g(x: Int): Stream[Int] = x #:: g(x + 1); g(2) }

  // define using corecursion an infinite stream of primes numbers starting with 2, i.e. [2,3,5,7,11,13,17,19,23,29,31...]
  val primes: Stream[Int] = {
    def g(n: Stream[Int]): Stream[Int] = n.head #:: g(n.filter( (x: Int) => x % n.head != 0 ))
    g(naturals)
  }

  // 5. Error handling using flatMap chaining with Either
  trait MiscellaneousError
  object ItemNotFoundError extends MiscellaneousError
  object TypeConversionError extends MiscellaneousError
  object UndefinedSlopeError extends MiscellaneousError
  val m: Map[String,Tuple4[String,String,String,String]] = Map(
    "slope one" -> ("0.0","0.0","1.0","1.0"), 
    "slope zero" -> ("1.0","1.0","2.0","1.0"), 
    "undefined slope" -> ("2.0","2.0","2.0","3.0"),
    "bad values" -> ("foo","bar","baz","biz")
  )
  val findValues: String => Either[MiscellaneousError,Tuple4[String,String,String,String]] = str =>
  try {
    Right(m(str))
  }
  catch { case e: Exception => Left(ItemNotFoundError) }


  val convertValues: Tuple4[String,String,String,String] => Either[MiscellaneousError,Tuple4[Double,Double,Double,Double]] = tuple =>
  try {
    Right(tuple._1.toDouble, tuple._2.toDouble, tuple._3.toDouble, tuple._4.toDouble)
  }
  catch {
    case e: Exception => Left(TypeConversionError)
  }

  val slope: Tuple4[Double,Double,Double,Double] => Either[MiscellaneousError,Double] = {
    case (x1, y1, x2, y2) => if (x2 - x1 != 0) Right((y2 - y1) / (x2 - x1)) else Left(UndefinedSlopeError)
}


  // DO NOT MODIFY THE FUNCTION BELOW
  def go = {
    println("Assignment 3")
    println("Task 1")
    println(s"For 1000 values, pi approximates to ${pi_approximation(values(Random)(1000))}")
    println(s"For 10000 values, pi approximates to ${pi_approximation(values(Random)(10000))}")
    println(s"For 100000 values, pi approximates to ${pi_approximation(values(Random)(100000))}")
    println(s"For 1000000 values, pi approximates to ${pi_approximation(values(Random)(1000000))}")
    println(s"Task 2")
    println(s"For 1000 streamed values, pi approximates to ${pi_approximation_option(streamed_values(Random))(1000).getOrElse(0.0)}")
    println(s"For 10 values but stream is empty, pi approximates to ${pi_approximation_option(Stream())(10).getOrElse(0.0)}")
    println(s"For 10 values but stream is not big enough, pi approximates to ${pi_approximation_option(Stream((0.3,0.4),(0.5,0.9)))(10).getOrElse(0.0)}")
    println(s"For 10 values using a List that isn't big enough, pi approximates to ${pi_approximation_option(List((0.2,0.3),(0.4,0.6)))(10).getOrElse(0.0)}")
    println("Task 3")
    println(s"All primes smaller than 30: ${SieveEratosthenes.allPrimes(30)}")
    println(s"Using options, all primes smaller than 30: ${allPrimesOption(Some(30))}")
    println(s"Using options, all primes smaller than None: ${allPrimesOption(None)}")
    println("Reverse linked list")
    val res1 = ReverseLinkedList.viewList(ReverseLinkedList.reverseListFunctional(
      new ListNode(1, new ListNode(2, new ListNode(3, new ListNode(4, null))))))
    println(s"Reversing linked list using original function: ${res1}")
    val res2 = reverseListOption(Some(new ListNode(1, new ListNode(2, new ListNode(3, new ListNode(4, null))))))
      .map(viewList)
    println(s"Reversing linked list using lifted function: ${res2}")
    println("Multiples of 3 or 5")
    println(s"Sum for n = 10, ${Multiples3Or5.getMultiples(10)}")
    println(s"Using lifted function, sum for n = 10: ${getMultiplesOption(Some(10))}")
    println("Bernoulli Trials")
    val res3 = BernoulliTrials.conductAllTrials(Random, 10, 10, 100)
    val res4 = conductAllTrialsOption(Some(Random), Some(10), Some(10), Some(100))
    println(s"Trial results for numTrials = 10, fewestFlips = 10, mostFlips = 100: ${res3}")
    println(s"Using lifted function, trial results for numTrials = 10, fewestFlips = 10, mostFlips = 100: ${res4}")
    println("Task 4")
    println("First ten factorials (manually computed): 1, 1, 2, 6, 24, 120, 720, 5040, 40320, 362880")
    println(s"First ten factorials (computed using corecursion): ${factorials.take(10).toList}")
    println("First ten Fibonacci numbers (manually computed): 1, 1, 2, 3, 5, 8, 13, 21, 34, 55")
    println(s"First ten Fibonacci numbers (computed using corecursion): ${fibs.take(10).toList}")
    println("First ten values of recurrence (manually computed): 2, 3, 5, 49, 160, 708, 2977, 12350, 51911, 217065")
    println(s"First ten values of recurrence t (computed using corecursion): ${t.take(10).toList}")
    println("First 10 natural numbers starting with 2: 2, 3, 4, 5, 6, 7, 8, 9, 10, 11")
    println(s"First 10 natural numbers starting with 2 (computed using corecursion): ${naturals.take(10).toList}")
    println("First 20 primes (computed manually): 2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61, 67, 71")
    println(s"First 20 primes (computed using corecursion): ${primes.take(20).toList}")
    println("Task 5")
    println(s"Slope one: ${findValues("slope one").flatMap(convertValues).flatMap(slope).fold(left => left.toString, right => right.toString)}")
    println(s"Slope zero: ${findValues("slope zero").flatMap(convertValues).flatMap(slope).fold(left => left.toString, right => right.toString)}")
    println(s"Invalid key: ${findValues("foo").flatMap(convertValues).flatMap(slope).fold(left => left.toString, right => right.toString)}")
    println(s"Bad values: ${findValues("bad values").flatMap(convertValues).flatMap(slope).fold(left => left.toString, right => right.toString)}")
    println(s"Undefined slope: ${findValues("undefined slope").flatMap(convertValues).flatMap(slope).fold(left => left.toString, right => right.toString)}")
  }
}
