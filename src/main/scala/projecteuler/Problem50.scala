package projecteuler
/**
Consecutive prime sum
Problem 50
The prime 41, can be written as the sum of six consecutive primes:

41 = 2 + 3 + 5 + 7 + 11 + 13
This is the longest sum of consecutive primes that adds to a prime below one-hundred.

The longest sum of consecutive primes below one-thousand that adds to a prime, contains 21 terms, and is equal to 953.

Which prime, below one-million, can be written as the sum of the most consecutive primes?
 */
object Problem50 {
  def solve(n: Int) = {
    val sieve = Sieve(n)
    def isPrime = sieve.isPrime _
    def maxConsecutiveSums(ls: List[Int], bound: Int): (Int, Int) = {
      var sum = ls.head
      var count = 1
      var remain = ls.tail
      var max = sum
      var num = 1
      while (!remain.isEmpty && sum + remain.head < bound)  {
        sum = sum + remain.head
        remain = remain.tail
        count = count + 1
        if (isPrime(sum)) {
          max = sum
          num = count
        }
      }
      (num, max)
    }
    def find(ls: List[Int], bound: Int): (Int, Int) = {
      var result = (1, ls.head)
      var remain = ls
      while (! remain.isEmpty) {
        val c = maxConsecutiveSums(remain, bound)
        result = if (c._1 > result._1) c else result
        remain = remain.tail
      }
      result
    }
    find(sieve.getPrimes.toList, 1000000)    
  }
  def main(args: Array[String]): Unit = {
    println(solve(1000000))
  }
}