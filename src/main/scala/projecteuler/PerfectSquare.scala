package projecteuler

trait PerfectSquare {
  def isPerfectSquare(n: BigInt) = {
    val r = BigDecimal(math.sqrt(n.toDouble)).toBigInt
    r * r == n
  }
}