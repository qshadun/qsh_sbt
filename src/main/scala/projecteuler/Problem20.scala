package projecteuler
/**
Factorial digit sum
Problem 20
n! means n  (n  1)  ...  3  2  1

For example, 10! = 10  9  ...  3  2  1 = 3628800,
and the sum of the digits in the number 10! is 3 + 6 + 2 + 8 + 8 + 0 + 0 = 27.

Find the sum of the digits in the number 100!
 */
object Problem20 {
  def factotrial(n: Int): BigInt = (2 to n).foldLeft(BigInt(1))(_ * _)
  def main(args: Array[String]): Unit = {
    println(factotrial(100).toString.toCharArray().map(_ - 48).sum)
  }

}