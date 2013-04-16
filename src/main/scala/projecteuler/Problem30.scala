package projecteuler
/**
Digit fifth powers
Problem 30
Surprisingly there are only three numbers that can be written as the sum of fourth powers of their digits:

1634 = 1^4 + 6^4 + 3^4 + 4^4
8208 = 8^4 + 2^4 + 0^4 + 8^4
9474 = 9^4 + 4^4 + 7^4 + 4^4
As 1 = 1^4 is not a sum it is not included.

The sum of these numbers is 1634 + 8208 + 9474 = 19316.

Find the sum of all the numbers that can be written as the sum of fifth powers of their digits.
 */
object Problem30 {

  def main(args: Array[String]): Unit = {
    /**
     * 9 ^ 5 = 59049,
     * 9 ^ 5 * 6 = 354294
     */
    def isDigPow(n: Int, p: Int) = 
      n.toString.toCharArray.map(_ - '0').foldLeft(0)(_ + Math.pow(_, p).toInt) == n
      val nums = (2 to 354294).filter(isDigPow(_, 5))
      println(nums)
      println(nums.sum)
  }

}