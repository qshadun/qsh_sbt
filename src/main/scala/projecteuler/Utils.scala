package projecteuler

object Utils {
  def gcd(a: BigInt, b: BigInt): BigInt = if (b == 0) a else gcd(b, a % b)
  def lcm(a: BigInt, b: BigInt): BigInt = a * b / gcd(a, b)
}
import Utils._
case class Rational(n: BigInt, d: BigInt) extends Ordered[Rational]{
  require(d != 0)
  def normalize = {
    val g = if (n < 0) gcd(-n, d) else gcd(n, d)
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
  def *(that: Rational) = Rational(this.n * that.n, this.d * that.d)
  def *(that: Int) = Rational(this.n * that, this.d)
  def /(that: Rational) = Rational(this.n * that.d, this.d * that.n)
  def /(that: Int) = Rational(this.n, this.d * that)
  def toDecimal = BigDecimal(n) / BigDecimal(d)
  override def toString = n + "/" + d
  override def compare(that: Rational) = {
    val diff = this.n * that.d - this.d * that.n
    if (diff < 0) -1
    else if (diff == 0) 0
         else 1
  }
}
object Rational {
  def apply(x: (Int, Int)) = new Rational(x._1, x._2)
  def apply(x: Int) = new Rational(BigInt(x), BigInt(1))
  def apply(x: BigInt) = new Rational(x, BigInt(1))
  def apply(x: Long) = new Rational(BigInt(x), BigInt(1))
  import scala.language.implicitConversions
  implicit def intToRational(x: Int) = Rational(x)
  implicit def BigIntToRational(x: BigInt) = new Rational(x, BigInt(1))
  implicit def LongToRational(x: Long) = new Rational(BigInt(x), BigInt(1))
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