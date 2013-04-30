package projecteuler

import scala.collection.mutable.ArrayBuffer
/**
 * Counting fractions
 * Problem 72
 * Consider the fraction, n/d, where n and d are positive integers. If nd and HCF(n,d)=1, it is called a reduced proper fraction.
 *
 * If we list the set of reduced proper fractions for d  8 in ascending order of size, we get:
 *
 * 1/8, 1/7, 1/6, 1/5, 1/4, 2/7, 1/3, 3/8, 2/5, 3/7, 1/2, 4/7, 3/5, 5/8, 2/3, 5/7, 3/4, 4/5, 5/6, 6/7, 7/8
 *
 * It can be seen that there are 21 elements in this set.
 *
 * How many elements would be contained in the set of reduced proper fractions for d <= 1,000,000?
 */
object Problem72 {
  def gcd(a: Int, b: Int): Int = if (b == 0) a else gcd(b, a % b)
  case class PhiSieve(bound: Int) {
    private val sieve = Array.fill(bound)(List.empty[Int])
    (2 until bound).foreach { x =>
      if (sieve(x).size == 0)
        ((x + x) until bound by x).foreach(i =>
          sieve(i) = x :: sieve(i))
    }
    def phi(n: Int) = {
      require(n < bound && n > 0)
      if (sieve(n).size == 0) n - 1
      else {
        val pf = sieve(n)
        (pf.foldLeft(BigInt(n))((acc, i) => acc * (i - 1)) / pf.foldLeft(BigInt(1))(_ * _)).toInt
      }
    }
  }
  def main(args: Array[String]): Unit = {
    val bound = 1000000
    val sieve = PhiSieve(bound + 1)
    def phi = sieve.phi _
    val result = (2 to bound).foldLeft(0L) { (acc, d) => acc + phi(d) }
    println(result)
  }
}