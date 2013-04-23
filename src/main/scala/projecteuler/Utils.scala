package projecteuler

object Utils {
  def gcd(a: BigInt, b: BigInt): BigInt = if (b == 0) a else gcd(b, a % b)
  def lcm(a: BigInt, b: BigInt): BigInt = a * b / gcd(a, b)
}
import Utils._
case class Rational(n: BigInt, d: BigInt) {
  require(d != 0)
  def normalize = {
    val g = gcd(n, d)
    new Rational(n / g, d / g)
  }
  def +(that: Rational) = {
    val l = lcm(this.d, that.d)
    Rational(this.n * (l / this.d) + that.n * (l / that.d), l)
  }
  def +(that: Int) = Rational(this.n + this.d, this.d)
  def -(that: Int) = Rational(this.n - this.d, this.d)
  def -(that: Rational) = {
    val l = lcm(this.d, that.d)
    Rational(this.n * (l / this.d) - that.n * (l / that.d), l)
  }
  def toDecimal = BigDecimal(n) / BigDecimal(d)
  override def toString = n + "/" + d
}
object Rational {
  def apply(x: (Int, Int)) = new Rational(x._1, x._2)
  def apply(x: Int) = new Rational(x, 1)
  def apply(x: Rational, y: Rational) = new Rational(x.n * y.d, x.d * y.n)
  implicit def intToRational(x: Int) = Rational(x)
}
case class Sieve(bound: Int) {
  private val sieve = Array.fill(bound)(0)
  (2 to math.sqrt(bound).toInt).foreach { x =>
    if (sieve(x) == 0)
      ((x + x) until bound by x).foreach { i =>
        sieve(i) = sieve(i) + 1
      }
  }
  lazy val getPrimes = for (i <- 2 until bound; if sieve(i) == 0) yield i
  def isPrime(n: Int) = {
    require(n < bound && n > 0)
    sieve(n) == 0
  }
}