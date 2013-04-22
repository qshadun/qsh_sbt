package projecteuler
/**
Permuted multiples
Problem 52
Published on Friday, 12th September 2003, 06:00 pm; Solved by 29485
It can be seen that the number, 125874, and its double, 251748, contain exactly the same digits, 
but in a different order.

Find the smallest positive integer, x, such that 2x, 3x, 4x, 5x, and 6x, contain the same digits.
 */
object Problem52 {
  def main(args: Array[String]): Unit = {
    def isSameDigits(a: Int, b: Int) = a.toString.sortBy(_.toInt) == b.toString.sortBy(_.toInt)
    println(Stream.from(1).find(x => isSameDigits(x, 2*x) && isSameDigits(x, 3*x) && isSameDigits(x, 4*x) && isSameDigits(x, 5*x) && isSameDigits(x, 6*x)))
  }
}