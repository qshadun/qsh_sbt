package projecteuler
/**
 * Power digit sum
 * Problem 16
 * 2^15 = 32768 and the sum of its digits is 3 + 2 + 7 + 6 + 8 = 26.
 *
 * What is the sum of the digits of the number 2^1000?
 */
object Problem16 {

  def main(args: Array[String]): Unit = {
    def twoToThePow(n: Int) = BigInt(2).pow(n)
//      (1 to n).foldLeft(BigInt(1)) { (acc, x) =>
//        2 * acc
//      }

    def sumOfDigits(n: BigInt) = 
      n.toString.toCharArray.map(_ - 48).sum
      
    println(sumOfDigits(twoToThePow(1000)))
  }

}