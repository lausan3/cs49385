import scala.::
import java.io.{PrintWriter, File}
import scala.util.Random


object BernoulliTrials {

  type REAL_RESULT = (Double, Double)
  type RESULTS_2D = List[REAL_RESULT]

  def go = {
    val numTrials = 1000
    val fewestFlips = 10
    val mostFlips = 2000
    val r = new Random()
    val results: RESULTS_2D = conductAllTrials(r, numTrials, fewestFlips, mostFlips)
    println(s"results = ${results}")
    val fileName = s"streak_length_for_num_flips_${numTrials}_${fewestFlips}_${mostFlips}.csv"
    val pw = new PrintWriter(new File(fileName))
    results.map(e => pw.write(s"${e._1},${e._2}\n"))
    pw.close()

  }

  def conductAllTrials(r: Random, numTrials: Int, fewestFlips: Int, mostFlips: Int): RESULTS_2D =
    (fewestFlips to mostFlips).foldLeft(List.empty[REAL_RESULT])( (acc: RESULTS_2D, flipAmount: Int) =>
      (flipAmount.toDouble, averageMaxStreakLength(r, numTrials, flipAmount)) :: acc)

  def averageMaxStreakLength(r: Random, numTrials: Int, flipsPerTrial: Int): Double =
    (1 to numTrials).foldLeft(List.empty[Int])( (acc: List[Int], trial: Int) =>
        longestStreak(r, flipsPerTrial) :: acc).reverse
      // After getting the trials, sum them up and get the average
      // I'm summing with foldLeft because List.sum implicitly returned an Int and couldn't be changed to
      // a Double.
      .foldLeft(0.0)( (acc: Double, longestStreak: Int) => acc + longestStreak ) / numTrials

  def longestStreak(r: Random, numFlips: Int): Int = (1 to numFlips).foldLeft((0, 0))( (acc: (Int, Int), x: Int) =>
    // acc: (Int, Int) = (currentStreak, maxStreak)
    if (r.nextBoolean()) (acc._1 + 1, acc._2)
      // If we hit a tails, we update maxStreak.
    else (0, acc._1 max acc._2)
  )._2
}
