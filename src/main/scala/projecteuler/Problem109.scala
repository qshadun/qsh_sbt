package projecteuler
//Darts
object Problem109 {
  val scores = List.tabulate(20)(_ + 1) :+ 25
  val combines3 = List(1, 2, 3).combinations(2).toList
  val combines3Same = List(1, 2, 3).map(List.fill(2)(_))
  val combines2 = List(1, 2, 3)
  def main(args: Array[String]): Unit = {
    var count = scores.size // check out with 1 throw
    for ( // checkout with 3 throws, first throw has lower rate than second, e.g., single vs double
      i <- scores;
      j <- scores;
      k <- scores;
      combo <- combines3;
      if !(i == 25 && combo.head == 3 || j == 25 && combo.last == 3);
      if i * combo.head + j * combo.last + k * 2 < 100
    ) {
      count = count + 1
    }
    for ( // checkout with 3 throws, first and second throws have same rate
      i <- scores;
      j <- scores.filter(_ >= i);
      k <- scores;
      combo <- combines3Same;
      if !(i == 25 && combo.head == 3 || j == 25 && combo.last == 3);
      if i * combo.head + j * combo.last + k * 2 < 100
    ) {
      count = count + 1
    }
    for (// checkout with 2 throws
      i <- scores;
      j <- scores;
      combo <- combines2;
      if !(i == 25 && combo == 3);
      if i * combo + j * 2 < 100
    ) {
      count = count + 1
    }
    println(count)
  }
}