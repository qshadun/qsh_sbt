package projecteuler
//Repunit divisibility
object Problem129 {
  def gcd(a: Int, b: Int): Int = if (b == 0) a else gcd(b, a % b)
  def bruteForce(n: Int) = {
    var x = 1
    while(BigInt(10).modPow(x, 9 * n) != 1) x = x + 1
    x
  }
  def solve(target: Int) = Stream.from(target + 1).find { x =>
    gcd(x, 10) == 1 && bruteForce(x) > target
  }
  def main(args: Array[String]) = {
    val target = if (args.isEmpty) 1000000 else args(0).toInt
    println(solve(target))
  }
}