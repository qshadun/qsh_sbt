package projectEuler
import scala.annotation.tailrec
/**
 * 10001st prime
 * Problem 7
 * By listing the first six prime numbers: 2, 3, 5, 7, 11, and 13, we can see that the 6th prime is 13.
 *
 * What is the 10 001st prime number?
 */

object Problem7 {

//  def primes: Stream[BigInt] = {
//
//    var is = Stream.iterate(BigInt(2)){_ + 1}
//
//    def sieve(numbers: Stream[BigInt]): Stream[BigInt] = {
//        numbers.head #:: sieve(numbers.filter(_ % numbers.head != 0))
//    }
//    sieve(is)
//  }

  import scala.collection.mutable.ArrayBuffer
  val primes: ArrayBuffer[BigInt] = ArrayBuffer(2, 3, 5, 7, 11, 13)
  
  def findNextPrime() {
    val next = Stream.iterate(primes.last + 2){_ + 2}.filter(x => primes.takeWhile(_ <= sqrt(x)).forall(x % _ != 0)).head
    primes.append(next)
  }
  def sqrt(number: BigInt) = {
    def next(n: BigInt, i: BigInt): BigInt = (n + i / n) >> 1
    val one = BigInt(1)
    var n = one
    var n1 = next(n, number)
    while ((n1 - n).abs > one) {
      n = n1
      n1 = next(n, number)
    }
    while (n1 * n1 > number) {
      n1 -= one
    }
    n1
  }
  
  // lazy way
  val primesLazy: Stream[Int] = {
    val findedBiggerThan2 = ArrayBuffer(3)
    def isPrime(n: Int) = 
      findedBiggerThan2.takeWhile(_ <= Math.sqrt(n).toInt).forall(n % _ != 0)
    def loop(s: Int): Stream[Int] = {
      if (s < 0) Stream.empty
      else if (!isPrime(s)) loop(s + 2)
      else {
        findedBiggerThan2.append(s)
        s #:: loop(s + 2)
      }
    }
    2 #:: 3 #:: loop(5)
  } 

  def main(args: Array[String]): Unit = {
//    val knownCount = primes.size
//    for (i <- knownCount until 10001)
//      findNextPrime()
//    println(primes.size)
//    println(primes(10000))
    val start = System.currentTimeMillis()
    println(primesLazy.take(10001).last)
    println("Spend " + (System.currentTimeMillis() - start) + " ms")
  }

}