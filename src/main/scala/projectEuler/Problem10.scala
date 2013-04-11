package projectEuler

/**
Summation of primes
Problem 10
Published on Friday, 8th February 2002, 06:00 pm; Solved by 117649
The sum of the primes below 10 is 2 + 3 + 5 + 7 = 17.

Find the sum of all the primes below two million.
 */
object Problem10 {

  import Problem7._
  def main(args: Array[String]): Unit = {
    println(primesLazy.takeWhile(_ < 10).sum)
    println(primesLazy.takeWhile(_ < 2000000).foldLeft(0L)(_ + _))
  }
}