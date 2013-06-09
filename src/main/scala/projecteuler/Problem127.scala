package projecteuler
//abc-hits
object Problem127 {
  /**
   * Four key points:
   *
   * First, use the hint about problem 124 and make a sorted list of radicals.
   * Second, it's easier to iterate on b, because then you have a bound on c = a + b (c < 2*b).
   * Third, since Rad(c) >=2, we must have Rad(a) < c / (Rad(c)*Rad(b)) < b / Rad(b).
   * Finally, by using the sorted list, we can efficiently only look at the values for a where Rad(a) fits within this bound.
   */
  case class PrimeFactorSieve(bound: Int) {
    private val sieve = Array.fill(bound)(Set.empty[Int])
    (2 until bound).foreach { x =>
      if (sieve(x).size == 0)
        (x + x until bound by x).foreach(i =>
          sieve(i) = sieve(i) + x)
    }
    def getPrimeFactors(n: Int) = if (n > 1 && sieve(n).isEmpty) Set(n) else sieve(n)
  }

  def main(args: Array[String]): Unit = {
    val limit = if (args.isEmpty) 1000 else args(0).toInt
    val sieve = PrimeFactorSieve(limit)

    import sieve._
    def isMutualPrime(a: Int, b: Int) = getPrimeFactors(a).forall(!getPrimeFactors(b).contains(_))
//    def radProduct(a: Int, b: Int, c: Int) = getPrimeFactors(a).product.toLong * getPrimeFactors(b).product * getPrimeFactors(c).product
    def rad(n: Int) = getPrimeFactors(n).product
    //    def abc(limit: Int) =
    //      (1 until limit / 2).foldLeft(0L) { (acc, a) =>
    //        (a + 1 until limit - a).foldLeft(0L) { (acc1, b) =>
    //          val c = a + b
    //          if (isMutualPrime(a, b) && isMutualPrime(a, c) && isMutualPrime(b, c) && radProduct(a, b, c) < c)
    //            acc1 + c
    //          else acc1
    //        } + acc
    //      }
    //    def abcHit(limit: Int) = (1 until limit / 2).map { a =>
    //      (a + 1 until limit - a).map { b =>
    //        val c = a + b
    //        if (isMutualPrime(a, b) && isMutualPrime(a, c) && isMutualPrime(b, c) && radProduct(a, b, c) < c)
    //          Some((a, b, c))
    //        else None
    //      }.flatten
    //    }.flatten
    //
    //    //A minor realization: GCD(a,b) = 1 implies GCD(a,c) = GCD(b,c) = 1 so there's no need to check them.
    //    def abc1(limit: Int) = (1 until limit).filter(c => rad(c) < c).foldLeft(0L) { (acc, c) =>
    //      val step = if (c % 2 == 0) 2 else 1
    //      (1 to c / 2 by step).filter(a => isMutualPrime(a, c - a) && radProduct(a, c - a, c) < c).size * c + acc
    //    }
    val abcHit = {
      val sortedRad = List.tabulate(limit - 1)(_ + 1).map(n => (rad(n), n)).sorted
      (2 until limit-1).foldLeft(0L) { (acc, b) =>
        val rb = rad(b)
        sortedRad.takeWhile(_._1 < b / rb).filter {
          case (ra, a) => a < b && b + a < limit && isMutualPrime(a, b)
        }.filter {
          case (ra, a) => ra.toLong * rb * rad(a + b) < a + b
        }.foldLeft(acc) {case (acc1, (ra, a)) =>
          acc1 + a + b
        }
      }
    }
    println(abcHit)
  }
}