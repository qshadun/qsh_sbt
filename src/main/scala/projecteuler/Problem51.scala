package projecteuler
/**
Prime digit replacements
Problem 51
By replacing the 1st digit of *3, it turns out that six of the nine possible values: 13, 23, 43, 53, 73, and 83, are all prime.

By replacing the 3rd and 4th digits of 56**3 with the same digit, this 5-digit number is the first example having seven primes among the ten generated numbers, 
yielding the family: 56003, 56113, 56333, 56443, 56663, 56773, and 56993. Consequently 56003, being the first member of this family, is the smallest prime with this property.

Find the smallest prime which, by replacing part of the number (not necessarily adjacent digits) with the same digit, is part of an eight prime value family.
 */
// You have to replace digits in groups of 3; otherwise, at least three of the new primes will be divisible by 3.
object Problem51 {
  def main(args: Array[String]): Unit = {
    val sieve = Sieve(10000000)
    val primes = sieve.getPrimes
    def isPrime = sieve.isPrime _
    def countPrime(ls: Seq[Int]) = ls.map(x => if (isPrime(x)) 1 else 0).sum
    def family3(n: Int) = {
      val digits = n.toString.toCharArray
      def findGroups: Seq[(Int, Int, Int)] = {
        if(digits.length <= 3) Nil
        else
          for(i <- 0 to digits.length - 4; 
            j <- i + 1 to digits.length - 3;
            if (digits(i) == digits(j));
            k <- j + 1 to digits.length - 2;
            if (digits(i) == digits(k))) yield(i + 1, j + 1, k + 1)
      }
      findGroups.map{case(i, j, k) =>
        val validNumbers = if (i == 1) 1 to 9 else 0 to 9
        validNumbers.map{x => 
          val p1 = digits.take(i - 1)
          val p2 = (x + '0').toChar +: digits.slice(i, j - 1)
          val p3 = (x + '0').toChar +: digits.slice(j, k - 1)
          val p4 = (x + '0').toChar +: digits.drop(k)
          (p1 ++ p2 ++ p3 ++ p4).mkString.toInt
        }
      }
    }
    def isFamily3(n: Int, c: Int) = {
      family3(n).find(countPrime(_) == c).isDefined
    }
    println(primes.find(isFamily3(_, 8)))
  }
}
