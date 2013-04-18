package projecteuler
/**
Goldbach's other conjecture
Problem 46
It was proposed by Christian Goldbach that every odd composite number can be written as the sum of a prime and twice 
a square.

9 = 7 + 2*1^2
15 = 7 + 2*2^2
21 = 3 + 2*3^2
25 = 7 + 2*3^2
27 = 19 + 2*2^2
33 = 31 + 2*1^2

It turns out that the conjecture was false.

What is the smallest odd composite that cannot be written as the sum of a prime and twice a square?
 */
object Problem46 {
  def main(args: Array[String]): Unit = {
    def isPrime(n: Int) = n == 2 || n > 2 && (2 to math.sqrt(n).toInt).forall(n % _ != 0)
    val oddComposites = Stream.from(3).filter(x => x % 2 != 0 && !isPrime(x))
    val twiceSquare = Stream.tabulate(Int.MaxValue)(x => 2 * x * x).tail
    val primes = Stream.from(2).filter(isPrime)
    val result = oddComposites.find{x =>
      val ps = primes.takeWhile(_ < x)
      val ts = twiceSquare.takeWhile(_ < x)
      ps.forall(a => ts.forall(a + _ != x))
    }
    println(result.get)
  }
}