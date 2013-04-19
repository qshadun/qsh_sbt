package projecteuler
/**
Prime permutations
Problem 49
The arithmetic sequence, 1487, 4817, 8147, in which each of the terms increases by 3330, 
is unusual in two ways: 
(i) each of the three terms are prime, and, (ii) each of the 4-digit numbers are permutations of one another.

There are no arithmetic sequences made up of three 1-, 2-, or 3-digit primes, exhibiting this property, 
but there is one other 4-digit increasing sequence.

What 12-digit number do you form by concatenating the three terms in this sequence?
 */
object Problem49 {
  def main(args: Array[String]): Unit = {
    def isPrime(n: Int) = n == 2 || n > 2 && (2 to math.sqrt(n).toInt).forall(n % _ != 0)
    val primes = Stream.from(2).filter(isPrime)
    val ps = primes.dropWhile(_ < 1000).takeWhile(_ < 10000).toList
    def findSeq(ls: List[Int]):List[(Int, Int, Int)] = {
      if (ls.size < 3) Nil
      else {
        val n = ls.head
        def findTwoNumbers(ns: List[Int]): List[(Int, Int)] = ns match {
          case x :: y :: _ =>
            val thirdNumber = ns.tail.find(_ - x == x - n)
            if(thirdNumber.isDefined) (x, thirdNumber.get) :: findTwoNumbers(ns.tail)
            else findTwoNumbers(ns.tail)
          case _ => Nil
        }
        val samedigits = ls.tail.filter(_.toString.toCharArray.sortBy(_.toInt).mkString == n.toString.toCharArray.sortBy(_.toInt).mkString)
        findTwoNumbers(samedigits).map(x => (n, x._1, x._2)) ::: findSeq(ls.tail)
      }
    }
    println(findSeq(ps))
  }
}