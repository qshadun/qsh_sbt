package projecteuler

import java.util.concurrent.atomic.AtomicInteger

object Problem154 {
  val limit = 200000
  val exp = 12
  val factors = Array.fill(limit + 1)((0, 0))
  1 to limit foreach { n =>
    val countTwo = countFactor(n, 2)
    val countFive = countFactor(n, 5)
    factors(n) = (countTwo, countFive)
  }

  def countFactor(n: Int, f: Int): Int = {
    def recur(r: Int, sofar: Int): Int =
      if (r % f != 0) sofar
      else recur(r / f, sofar + 1)
    recur(n, 0)
  }

  val factorialFactors = factors.tail.scanLeft((0, 0)) {
    case ((at, af), (t, f)) => (at + t, af + f)
  }
  val (twoInLimit, fiveInLimit) = (factorialFactors(limit)._1 - exp, factorialFactors(limit)._2 - exp)
  def solve: Int = {
    var count = new AtomicInteger(0)
    (0 to limit / 3).par.foreach { x =>
      x to (limit - x) / 2 foreach { y =>
        val z = limit - x - y
        val totalTwo = factorialFactors(x)._1 + factorialFactors(y)._1 + factorialFactors(z)._1
        val totalFive = factorialFactors(x)._2 + factorialFactors(y)._2 + factorialFactors(z)._2
        if (totalTwo <= twoInLimit && totalFive <= fiveInLimit) {
          if (x == y && x == z) count.getAndAdd(1)
          else if (x == y || x == z || y == z) count.getAndAdd(3)
          else count.getAndAdd(6)
        }
      }
    }
    count.get
  }
    
  def main(args: Array[String]): Unit = {
    val start = System.currentTimeMillis()
    println(solve)
    println(s"Used ${System.currentTimeMillis() - start}ms")
  }
}