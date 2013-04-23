package projecteuler
/**
Powerful digit sum
Problem 56
A googol (10^100) is a massive number: one followed by one-hundred zeros; 100^100 is almost unimaginably large: 
one followed by two-hundred zeros. Despite their size, the sum of the digits in each number is only 1.

Considering natural numbers of the form, a^b, where a, b  100, what is the maximum digital sum?
 */
object Problem56 {
  def main(args: Array[String]): Unit = {
    def powDigitSum(a: Int, b: Int) = BigInt(a).pow(b).toString.toCharArray.map(_ - '0').sum
    println((1 to 100).foldLeft((1, 1, 1)){(acc, x) =>
      (1 to 100).foldLeft(acc){(acc1, y) =>
        val ds = powDigitSum(x, y)
        if (ds > acc1._3) (x, y, ds) else acc1
      }
    })
  }
}