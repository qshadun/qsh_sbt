package projecteuler

object Problem6 {

  /**
   * Sum square difference
Problem 6
The sum of the squares of the first ten natural numbers is,

12 + 22 + ... + 102 = 385
The square of the sum of the first ten natural numbers is,

(1 + 2 + ... + 10)2 = 552 = 3025
Hence the difference between the sum of the squares of the first ten natural numbers and the square of the sum is 3025  385 = 2640.

Find the difference between the sum of the squares of the first one hundred natural numbers and the square of the sum.
   */
  def findDiff(n: Int): Long = {
    val seq = 1 to n
    1L * seq.sum * seq.sum - seq.foldLeft(0L) {(acc, x) => acc + x.toLong * x.toLong} 
  }
  
    /**
     * (1 + 2 + ... + n)^2 = n^2 * (n+1)^2 * 1/4

    1^2 + 2^2 + ... + n^2 = n * (n+1) * (2n+1) * 1/6
     */

  def findDiff1(n: Int): Long = {
    val sum1 = 1L * n * n * (n + 1) * (n + 1) / 4
    val sum2 = 1L * n * (n + 1) * (2 * n + 1) / 6
    sum1 - sum2
  }
  def main(args: Array[String]): Unit = {
    println(findDiff(10))
    println(findDiff(100))
    println(findDiff1(10))
    println(findDiff1(100))

  }

}