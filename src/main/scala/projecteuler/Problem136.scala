package projecteuler
//Singleton difference
object Problem136 {
  def main(args: Array[String]): Unit = {
    import util.control.Breaks._
    val limit = 50000000
    val maxDelta = math.sqrt(3 * limit).toInt + 1
    val maxX = limit * 3 / 4 + 2
    val ar = Array.fill(limit)(0)
    (1 to maxX).foreach { x =>
      val start = x / 3 + 1
      breakable {
        (start to start + maxDelta).foreach { d =>
          val n = 3L * d * d + 2L * d * x - 1L * x * x
          if (n < limit) ar(n.toInt) = ar(n.toInt) + 1
          else break
        }
      }
    }
    val result = ar.filter(_ == 1).size
    println(result)
  }
}