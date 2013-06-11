package projecteuler

object MultiplicativeOrder {
  val sieve = Sieve(1000000)
  val primes = sieve.getPrimes
  def isPrime(n: Int) = if (n < 1000000) sieve.isPrime(n) else BigInt(n).isProbablePrime(20)
  def primeDivisions(n: Int): List[(Int, Int)] = {
    def oneFactor(x: Int, pm: Int) = {
      def recur(remain: Int, count: Int): (Int, Int) =
        if (remain % pm != 0) (count, remain)
        else recur(remain / pm, count + 1)
      recur(x, 0)
    }
    def recur(remain: Int, pmToCheck: Seq[Int]): List[(Int, Int)] =
      if (remain == 1) Nil
      else {
        val newPmToCheck = pmToCheck.dropWhile(remain % _ != 0)
        val (e, newRemain) = oneFactor(remain, newPmToCheck.head)
        (newPmToCheck.head, e) :: recur(newRemain, newPmToCheck.tail)
      }
    if (isPrime(n)) List((n, 1))
    else recur(n, primes)
  }
  def gcd(a: Int, b: Int): Int = if (b == 0) a else gcd(b, a % b)
  def lcm(a: Int, b: Int): Int = a * b / gcd(a, b)
  /**
   * http://rosettacode.org/wiki/Multiplicative_order
   * def multOrder_(a, p, k)
   * pk = p ** k
   * t = (p - 1) * p ** (k - 1)
   * r = 1
   * for q, e in t.prime_division
   * x = powerMod(a, t / q ** e, pk)
   * while x != 1
   * r *= q
   * x = powerMod(x, q, pk)
   * end
   * end
   * r
   * end
   *
   * def multOrder(a, m)
   * m.prime_division.inject(1) {|result, f|
   * result.lcm(multOrder_(a, *f))
   * }
   * end
   */
  def multOrder_(a: Int, p: Int, k: Int) = {
    val pk = math.pow(p, k).toInt
    val t = (p - 1) * math.pow(p, k - 1).toInt
    var r = 1
    primeDivisions(t).foreach {
      case (q, e) =>
        var x = BigInt(a).modPow(t / math.pow(q, e).toInt, pk)
        while (x != 1) {
          r = r * q
          x = x.modPow(q, pk)
        }
    }
    r
  }
  def multOrder(a: Int, m: Int) = {
    primeDivisions(m).map { case (p, k) => multOrder_(a, p, k) }.reduceLeft(lcm)
  }
}