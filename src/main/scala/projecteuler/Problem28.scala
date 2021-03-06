package projecteuler
/**
Number spiral diagonals
Problem 28
Starting with the number 1 and moving to the right in a clockwise direction a 5 by 5 spiral is formed as follows:

21 22 23 24 25
20  7  8  9 10
19  6  1  2 11
18  5  4  3 12
17 16 15 14 13

It can be verified that the sum of the numbers on the diagonals is 101.

What is the sum of the numbers on the diagonals in a 1001 by 1001 spiral formed in the same way?
 */
object Problem28 {

  /**
   * Numbers in diagonals are:
   * 1
   * 3 5 7 9       3*3, step is 2
   * 13 17 21 25   5*5, step is 4
   */
  def main(args: Array[String]): Unit = {
    val diagNumbers = (3 to 1001 by 2).foldLeft(List(List(1))) {(acc, x) =>
      val numberInThisLevel = (1 to 4).map(acc.head.last + (x - 1) * _).toList
      numberInThisLevel :: acc
    }.reverse.flatten
//    println(diagNumbers)
    println(diagNumbers.foldLeft(0L)(_ + _))
  }

}