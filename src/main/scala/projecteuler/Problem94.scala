package projecteuler
object Problem94 {
  def main(args: Array[String]): Unit = {
    import scala.collection.mutable
    val edges = mutable.Set.empty[(Long, Long)]
    val bound = 1000000000L
    val vMax = math.sqrt(bound / 2).toLong
    for (v <- 2L to vMax) {
      val start = if (v % 2 == 0) 1L else 2L 
        for (u <- start until v by 2) {
          val x = v * v - u * u
          val y = 2 * v * u
          val z = v * v + u * u
          if (math.abs(2 * x - z) == 1) edges.add((z, x))
          if (math.abs(2 * y - z) == 1) edges.add((z, y))
        }
    }
    println(edges.foldLeft(0L){(acc, x) =>
      val l = 2 * (x._1 + x._2) 
      if (l <= bound) acc + l else acc 
    })
  }
}