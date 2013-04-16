package projecteuler
/**
Pandigital prime
Problem 41
We shall say that an n-digit number is pandigital if it makes use of all the digits 1 to n exactly once. 
For example, 2143 is a 4-digit pandigital and is also prime.

What is the largest n-digit pandigital prime that exists?
 */
object Problem41 {
  def main(args: Array[String]): Unit = {
    def isPrime(n: Int) = n == 2 || n > 2 && (2 to Math.sqrt(n).toInt).forall(n % _ != 0)
//    def combines(n: Int): List[Int] = {
//      require(n < 10 && n > 0)
//      val digits = (1 to n).toList.reverse
//      def combineR(used: List[Int], pos: Int, remain: List[Int]): List[Int] = 
//        if (pos == n) List(used.reverse.mkString.toInt)
//        else remain.map{x =>
//          combineR(x :: used, pos + 1, remain - x)
//        }.flatten
//      combineR(Nil, 0, digits)
//    } 
    def find(start: Int): Int = {
      val num = (start to 1 by -1).permutations.map(_.mkString.toInt).find(isPrime).getOrElse(0)
      if (num > 0) num else find(start - 1)
    }
    // only 1, 4 and 7 need to be considered, because n(n+1)/2 will be dividable by 3 otherwise
    println(find(7))
  }
}