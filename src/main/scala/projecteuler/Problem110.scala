package projecteuler
/**
If n = (p1^a1)(p2^a2)...(pt^at), a(n) = ((2 * a1 + 1)(2 * a2 + 1) ... (2 * at + 1) + 1)/2. (http://oeis.org/A018892)
p1, p2, ..., pt are prime factors of n.
So the base solution for this problem is the product of the first 15 primes (3 ^ 15 > 8000000)
Then I tried to decrease the number of prime factors to 14. 
Start from a1 = a2 = ... = a14 = 1, increase the exponents heuristically and find the smallest number which has more than 4000000 solutions.
Then find the smallest number for 13 prime factors until I cannot find a smaller number.
 */
object Problem110 {
  val sieve = Sieve(100)
  val primes = sieve.getPrimes
  def countSolutions(exps: List[Int]) = (exps.map(2 * _ + 1).map(BigInt.apply).product + 1) / 2
  def toNumber(exps: List[Int]) = exps.zipWithIndex.map { case (a, p) => BigInt(primes(p)).pow(a) }.product
  def findSmallest(n: Int, bound: Int) = { // n is the number of prime factors
    import scala.collection.mutable
    val m = mutable.Map.empty[Int, Int]
    val ps = primes.take(n)
    ps.tail.foreach { p => // 3 > 2, 5 > 2, 7 > 2 * 3
      var i = 1
      while (ps.take(i).product < p) i = i + 1
      m(p) = i
    }
    val start = List.fill(n)(1)
    def recur(exps: List[Int], i: Int): List[Int] = {
      if (countSolutions(exps) >= bound) exps
      else {
        if (i == 0) recur(exps.head + 1 :: exps.tail, 1)
        else {
          val s1 = {
            val newExps = exps.take(i) ::: exps(i) + 1 :: exps.drop(i + 1)
            recur(newExps, (i + 1) % n)
          }
          val smalls = m(primes(i))
          val s2 = recur(exps.take(smalls).map(_ + 1) ::: exps.drop(smalls), i)
          if (toNumber(s1) < toNumber(s2)) s1 else s2
        }
      }
    }
    recur(start, 0)
  }
  def solve(bound: Int) = {
    var n = Stream.from(1).find(BigInt(3).pow(_) > bound).get
    var result = toNumber(findSmallest(n, bound))
    var continue = true
    while (continue) {
      n = n - 1
      val newResult = toNumber(findSmallest(n, bound))
      if (newResult < result)
        result = newResult
      else continue = false
    }
    result
  }
  def main(args: Array[String]): Unit = {
    val bound = if (args.length > 0) args(0).toInt else 4000000
    println(solve(bound))
  }
}