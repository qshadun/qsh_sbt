package projecteuler
/**
Distinct primes factors
Problem 47
The first two consecutive numbers to have two distinct prime factors are:

14 = 2 * 7
15 = 3 * 5

The first three consecutive numbers to have three distinct prime factors are:

644 = 2^2 * 7 * 23
645 = 3 * 5 * 43
646 = 2 * 17 * 19.

Find the first four consecutive integers to have four distinct prime factors. What is the first of these numbers?
 */
object Problem47 {

  def main(args: Array[String]): Unit = {
//    def isPrime(n: Int) = n == 2 || n > 2 && (2 to math.sqrt(n).toInt).forall(n % _ != 0)
//    val primes = Stream.from(2).filter(isPrime)
//    def numOfPrimeFactors(n : Int) = 
//      primes.takeWhile(_ <= n / 2).filter(n % _ == 0).size
    def numOfPrimeFactors(n : Int) = {
      var count = 0;
      var remain = n;
      var start = 2;
      while (remain != 1) {
        if (remain % start == 0) {
          count = count + 1;
          while (remain % start == 0) remain = remain / start
        }
        start = start + 1
      }
      count
    }
    def find(start: Int, consecutive: Int): Int = 
      if ((0 until consecutive).map(start + _).forall(numOfPrimeFactors(_) == consecutive)) start
      else (find(start + 1, consecutive))
//    println(find(1, 4))
      println(sieve)
  }
  
  def sieve = {
    val lim = 1000000
    val sieve = Array.fill(1000000)(0)
    val sec = 4
    val expect = List.fill(sec)(sec)
    (2 to math.sqrt(lim).toInt).foreach{x =>
      if (sieve(x) == 0)
        ((x + x) until lim by x).foreach {i => 
          sieve(i) = sieve(i) + 1
      }
    }
    (2 until lim - sec).find(x => sieve.slice(x, x + sec).toList == expect).get
  }

}