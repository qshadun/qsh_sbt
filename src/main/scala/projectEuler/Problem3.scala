package projectEuler

/**
 * Largest prime factor
Problem 3
The prime factors of 13195 are 5, 7, 13 and 29.

What is the largest prime factor of the number 600851475143 ?
 */
object Problem3 {

  def isPrime(n: Long) = {
    (2L to Math.sqrt(n).toLong).forall(n % _ != 0) 
  }
  
  def findLargestFactor(n: Long) = {
    //lazy val naturals: Stream[Int] = Stream.cons(1, naturals.map(_ + 1))
    val naturals = Stream.iterate(1)(_ + 1)
    var theNum = n
    naturals.drop(1).dropWhile(n => {while(theNum % n == 0) {theNum /= n}; theNum > 1}).head
  }
  
  def main(args: Array[String]): Unit = {
    val n = 600851475143L;
//    val primes = (2L to Math.sqrt(n).toLong).filter(isPrime)
//    println((primes).filter(n % _ == 0).last)
//    
    println(findLargestFactor(n))
  }

}