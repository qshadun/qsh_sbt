package projecteuler
/**
Circular primes
Problem 35
The number, 197, is called a circular prime because all rotations of the digits: 197, 971, and 719, 
are themselves prime.

There are thirteen such primes below 100: 2, 3, 5, 7, 11, 13, 17, 31, 37, 71, 73, 79, and 97.

How many circular primes are there below one million?
 */
object Problem35 {
  def isPrime(n: Int) = n == 2 || (2 to Math.sqrt(n).toInt).forall(n % _ != 0)
//  def primes = Stream.from(2).filter(isPrime)
  def main(args: Array[String]): Unit = {
    val allPrimeCandidates = (2 until 1000000).filter(isPrime)
    def rotations(n: Int) = {
      val nStr = n.toString
      val rots = for (
        i <- 0 to nStr.length - 1;
        r = nStr.slice(i, nStr.length) + nStr.slice(0, i)
      ) yield(r.toInt)
      rots.filter(_.toString.length == nStr.length)
    }
    // can only composite from 1,3,7,9 for numbers bigger than 10
    def possible(n: Int) = n < 10 || n.toString.toCharArray.forall(List('1','3','7','9').contains(_))
    val allCircular = allPrimeCandidates.filter(possible).filter(rotations(_).forall(isPrime))
    println(allCircular)
    println(allCircular.size)
  }

}