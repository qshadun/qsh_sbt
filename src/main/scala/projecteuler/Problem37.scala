package projecteuler
/**
Truncatable primes
Problem 37
The number 3797 has an interesting property. Being prime itself, it is possible to continuously 
remove digits from left to right, and remain prime at each stage: 3797, 797, 97, and 7. 
Similarly we can work from right to left: 3797, 379, 37, and 3.

Find the sum of the only eleven primes that are both truncatable from left to right and right to left.

NOTE: 2, 3, 5, and 7 are not considered to be truncatable primes.
 */
object Problem37 {
  def isPrime(n: Int) = n == 2 || n > 2 && (2 to Math.sqrt(n).toInt).forall(n % _ != 0)
  def primes = Stream.from(2).filter(isPrime)
  def isTruncatablePrime(n: Int) = {
    def leftTrunPrime(s: String) = (0 to s.length - 1).map(s.substring(_).toInt).forall(isPrime)
    def rightTrunPrime(s: String) = (1 to s.length).map(s.substring(0, _).toInt).forall(isPrime)
    n > 10 && leftTrunPrime(n.toString) && rightTrunPrime(n.toString)
  }
  def main(args: Array[String]): Unit = {
    val result = primes.filter(isTruncatablePrime).take(11).toList
    println(result)
    println(result.sum)
  }

}