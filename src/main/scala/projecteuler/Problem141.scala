package projecteuler
/**
 * r < d, let d < q( they are interchangable), so r < d < q.
 * let d = rk and q = rk^2. k > 1 and k is rational.
 * d must small than sqrt(n).
 * let k = a / b and gcd(a, b) = 1, we got d = ra/b and q = ra^2/b^2, we can see b^2 must divide r.
 * let r = cb^2, we got d = cab and q = ca^2.
 * n = dq + r => n = a^3bc^2 + b^2c
 * because a is cubed, so a < 10000
 * 2 <= a < 10000, 1 <= b < a, c >= 1
 */
object Problem141 {
//  def progressive(n: Int) = {
//    val d = 2 to math.sqrt(n).toInt find {d =>
//      val r = n % d
//      val q = n / d
//      r != 0 && (d * d == r * q || d * r == q * q) 
//    }
//    if (d.isDefined) Some(n, n % d.get, d.get, n /d.get)
//    else None
//  }
  def isPerfectSquare(n: Long) = {
    val r = math.sqrt(n).toLong
    r * r == n
  }
  def find(limit: Long) = {
    val bound = math.cbrt(limit).toInt
    def f(a: Int, b: Int, c: Int) = a.toLong * a * a * b * c * c + b.toLong * b * c
    val results = for (
      a <- 2 until bound;
      b <- 1 until a
    ) yield {
      Stream.from(1).map(c => f(a, b, c)).takeWhile(_ < limit).filter(isPerfectSquare).toList
    }
    results.flatten.distinct
  }
  def main(args: Array[String]): Unit = {
    val limit = BigInt(10).pow(12).toLong
    println(find(limit).sum)
//    val numbersToCheck = 3 to math.sqrt(limit).toInt map (x => x * x)
//    val result = numbersToCheck.map(progressive).filter(_.isDefined).map(_.get)
//    println(result)
//    println(result.map(_._1).sum)
  }
}