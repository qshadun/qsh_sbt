package projecteuler

object Problem36 {
/**
Double-base palindromes
Problem 36
The decimal number, 585 = 10010010012 (binary), is palindromic in both bases.

Find the sum of all numbers, less than one million, which are palindromic in base 10 and base 2.

(Please note that the palindromic number, in either base, may not include leading zeros.)
 */
  def main(args: Array[String]): Unit = {
    val result = (1 until 1000000).filter(x => x.toString == x.toString.reverse && x.toBinaryString == x.toBinaryString.reverse)
    println(result)
    println(result.sum)
  }

}