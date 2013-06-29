package projecteuler
//Same differences
/**
 * (x+2d)^2 - (x+d)^2 - x^2 = n
 * 0 < n < 1000000 =>  x / 3   <  d <= (x + sqrt(3 * 1000000)) / 3
 * To find the max value of x, let x = 3k, 3k + 1, 3k + 2, the min value of n(when d = k + 1) is respectly 3, 8k +4, 4k + 3
 * 4k + 3 < 1000000 => k < 250000 => x < 3 * 250000 + 2
 */
object Project135 {
  def main(args: Array[String]): Unit = {
    val limit = 1000000
    val maxDelta = math.sqrt(3 * limit).toInt + 1
    val maxX = limit * 3 / 4 + 2
    val ar = Array.fill(limit)(0)
    (1 to maxX).foreach { x =>
      val start = x / 3 + 1
      (start to start + maxDelta).foreach { d =>
        val n = 3L * d * d + 2L * d * x - 1L * x * x
        if (n < limit) ar(n.toInt) = ar(n.toInt) + 1
      }
    }
    val result = ar.filter(_ == 10).size
    println(result)
  }
}