package projecteuler
//Modified Fibonacci golden nuggets
// 5 * n^2 + 14 * n + 1 is perfect square
object Problem140 extends PerfectSquare {
  def main(args: Array[String]): Unit = {
    //    val r = (1 to 1000000).filter(x => isPerfectSquare(f(x))).toList
    //    println(r)
    //    val ratio = 1 to (r.size -1) map{i =>
    //      r(i).toDouble / r(i - 1)
    //    }
    //    println(ratio)
    def f(n: BigInt) = 5 * n * n + 14 * n + 1
    def check(n: BigInt) = isPerfectSquare(f(n))
    def search(start: BigInt): BigInt = {
      if (check(start)) start
      else {
        var finded = false
        var result = start
        var delta = BigInt(1)
        while (!finded) {
          if (check(start + delta)) {
            finded = true
            result = start + delta
          } else if (check(start - delta)) {
            finded = true
            result = start - delta
          } else delta = delta + 1
        }
        result
      }
    }
    val alreadyFind = List(2, 5, 21, 42, 152, 296, 1050, 2037, 7205, 13970, 49392, 95760, 338546, 656357).map(BigInt.apply)
    val result = Array.fill(31)(BigInt(0))
    (0 to 13).foreach(i => result(i + 1) = alreadyFind(i))
    15 to 30 foreach { i =>
      val ratio = result(i - 2).toDouble / result(i - 3).toDouble
      val startPoint = BigDecimal(result(i - 1).toDouble * ratio).toBigInt
      result(i) = search(startPoint)
    }
    println(result(20))
    println(result.sum)
  }
}