package projecteuler
// Fibonacci golden nuggets
/**
(x + x^2)A(x) = A(x) - x
A(x) = x / (1 - x - x^2)
http://oeis.org/A081018
a(n) = 8a(n-1)-8a(n-2)+a(n-3)
F(3) + F(7) + F(11) +...+ F(4n+3).
 */
object Problem137 {
  def fibo: Stream[BigInt] = {
    def loop(a: BigInt, b: BigInt): Stream[BigInt] =  a #:: loop(b, a + b)
    loop(0, 1)
  }
  def nuggets: Stream[BigInt] = {
    def loop(nugget: BigInt, n: Int): Stream[BigInt] = nugget #:: loop(nugget + fibo(4 * n + 3), n + 1)
    loop(0, 0)
  }
  def solve(n: Int) = (0 to n).map(x => fibo(4 * x + 3)).sum
  def main(args: Array[String]): Unit = {
    println(nuggets.take(16).toList)
  }
}