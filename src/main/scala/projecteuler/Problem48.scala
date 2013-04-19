package projecteuler
/**
Self powers
Problem 48
The series, 1^1 + 2^2 + 3^3 + ... + 10^10 = 10405071317.

Find the last ten digits of the series, 1^1 + 2^2 + 33 + ... + 1000^1000.
 */
object Problem48 {
  def main(args: Array[String]): Unit = {
    val bound = 10000000000L;
    println((1 to 1000).map(x => BigInt(x).pow(x) % bound).sum % bound)
  }
}