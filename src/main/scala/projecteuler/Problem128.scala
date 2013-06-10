package projecteuler
//Hexagonal tile differences
/**
 * 1 has 3 prime differences; none of the rest of the center
 * seven cells does.
 *
 * Every cell n except for the first and last cells in a ring
 * will have the cells n-1 and n+1 as neighbors, and the
 * remaining cells will include two odds and two evens -
 * hence, the differences won't include three primes.
 *
 * So it remains only to check the first and last cell in
 * each ring.
 *
 * Edit: The way you prove this for the corner cells is:
 *
 * For the r'th corner cell in a ring, (1 <= r <= 5, r=0
 * is the first cell in the ring, which is a special case),
 * the differences between the cell and its neighbours are:
 *
 * -(6n+r), -1, 1, 6n+5+r, 6n+6+r, and 6n+7+r.
 *
 * Considering residues modulo 6, we get:
 *
 * r, (don't care), (don't care), r-1, r, and r+1
 *
 * Two of these are even, and two are odd, so there can't be
 * three primes among them.
 *
 */
object Problem128 {
  val sieve = Sieve(1000000)
//  def isPrime(n: Long) = sieve.isPrime(n.toInt)
  //  def find(n: Int) = {
  //    def countPrime(pre: Array[(Long, Int)], now: Array[(Long, Int)], i: Int, j: Int) =
  //      if (isPrime(now(i)._1 - pre(j)._1)) {
  //        pre(j) = (pre(j)._1, pre(j)._2 + 1)
  //        now(i) = (now(i)._1, now(i)._2 + 1)
  //      }
  //
  //    def recur(pre: Array[(Long, Int)], now: Array[(Long, Int)], count: Int): Long = {
  //      val level = now.length / 6
  //      now.zipWithIndex.foreach {
  //        case ((num, pc), i) =>
  //          if (i % level == 0){
  //            countPrime(pre, now, i, i - i / level)
  //          }
  //            
  //          else {
  //            countPrime(pre, now, i, i - i / level - 1)
  //            countPrime(pre, now, i, (i - i / level) % pre.length)
  //          }
  //      }
  //      val newCount = count + pre.filter(_._2 == 3).size
  //      if (newCount >= n) {
  //        val t = pre.filter(_._2 == 3)
  //        t(t.size - newCount + n - 1)._1
  //      } else {
  //        if (isPrime(now.length - 1)) {
  //          now(0) = (now(0)._1, now(0)._2 + 1)
  //          now(now.length - 1) = (now(now.length - 1)._1, now(now.length - 1)._2 + 1)
  //        }
  //        recur(now, Array.tabulate(now.length + 6)(i => (now.last._1 + 1 + i, 0)), newCount)
  //      }
  //    }
  //    recur(Array((2, 1), (3, 1), (4, 1), (5, 0), (6, 1), (7, 1)), (8 to 19).map(x => (x.toLong, 0)).toArray, 1)
  //  }

  def find(n: Int): Long =
    if (n == 1) 1L
    else {
      def recur(x: Long, level: Int, remainCount: Int):Long = {
        val firstIsPrime = {
          val diffs1 = List(level * 6 - 1, level * 6 + 1, level * 12 + 5)
          diffs1.forall(sieve.isPrime)
        }
        val lastIsPrime = if (level > 1) {
            val diffs2 = List(level * 6 - 1, level * 6 + 5, level * 12 - 7)
            diffs2.forall(sieve.isPrime)
        } else false
        val pc = {if(firstIsPrime) 1 else 0} + {if(lastIsPrime) 1 else 0}
        if (remainCount == 2 && pc == 2)
          x + 6 * level - 1
        else if (remainCount == 1 && pc > 0) {
          if (firstIsPrime) x else x + 6 * level - 1
        } else
           recur(x + 6 * level, level + 1, remainCount - pc)
      }
      recur(2L, 1, n - 1)
    }
  def main(args: Array[String]): Unit = {
    val ith = if (args.isEmpty) 2000 else args(0).toInt
    println(find(ith))
  }
}