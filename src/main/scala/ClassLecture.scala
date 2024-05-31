object ClassLecture {

  def go = {
    // Example showing how foldLeft lets us maintain state using a tuple
    val result = (1 to 10).foldLeft((0, List.empty[Int], 0, List.empty[Int]))({
      case ((cnt2: Int, lst2: List[Int], cnt3: Int, lst3: List[Int]), x: Int) => 
        update(x, cnt2, lst2, cnt3, lst3)
    })
    println(s"Counts and lists of multiples of two and three = ${result}")

    // Given a list of numbers, generate a list of their squares
    val result2 = (1 to 10).foldLeft(List.empty[Int])({
      case(sqlst: List[Int], x: Int) => (x*x) :: sqlst 
    }).reverse
    println(s"Result = ${result2}")

    // Simple example using flatMap
    val result3 = (1 to 10).flatMap(x => List(x*x))
    println(s"Same result using flatMap = ${result3}")
  }

  def update(x: Int, cnt2: Int, lst2: List[Int], cnt3: Int, lst3: List[Int]) = 
    if (x % 2 == 0 && x % 3 == 0) (cnt2 + 1, x :: lst2, cnt3 + 1, x :: lst3)
    else if (x % 2 == 0) (cnt2 + 1, x :: lst2, cnt3, lst3)
    else if (x % 3 == 0) (cnt2, lst2, cnt3 + 1, x :: lst3)
    else (cnt2, lst2, cnt3, lst3)

}
