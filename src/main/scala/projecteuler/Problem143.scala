package projecteuler
/**
 * The angles ATB=ATC=BTC=120, because in an triangle with edge length a, b, c and the angle facing c is alpha, we
 * have c^2 = a^2 + b^2 - 2 * a * b * cos(alpha). So
 * c^2 = p^2 + r^2 + pr
 * a^2 = q^2 + r^2 + qr
 * b^2 = p^2 + q^2 + pq
 */
object problem143 {
  def isPerfectSquare(n: Long) = {
    val root = math.sqrt(n).toLong
    root * root == n
  }
  def foo(p: Long, q: Long) = p * p + q * q + p * q
  def check(p: Int, q: Int) = isPerfectSquare(foo(p, q))
  //suppose p <= q <= r, so p < limit /3
  def find(limit: Int) = 
    for (
      p <- 1 to limit / 3;
      q <- p to  (limit - p) / 2;
      if check(p, q);
      r <- q to limit - p - q;
      if check(p, r) && check(q, r)
    ) yield(p + q + r)
  
  def main(args: Array[String]): Unit = {
    val limit = 120000
    println(find(limit).distinct.sum)
  }
}