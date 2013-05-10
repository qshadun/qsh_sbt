package projecteuler
// Right triangles with integer coordinates
object Problem91 {
  def check(p1: (Int, Int), p2: (Int, Int)): Boolean = {
    val op1Sq = p1._1 * p1._1 + p1._2 * p1._2
    val op2Sq = p2._1 * p2._1 + p2._2 * p2._2
    val dx = p1._1 - p2._1
    val dy = p1._2 - p2._2
    val p1p2Sq = dx * dx + dy * dy
    val edgeSqs = List(op1Sq, op2Sq, p1p2Sq)
    val max = edgeSqs.max
    max == edgeSqs.sum / 2
  }
  def main(args: Array[String]): Unit = {
    val bound = 50
    val r = for(
          x1 <- 0 to bound;
          y1 <- 0 to bound;
          x2 <- 0 to bound;
          y2 <- 0 to bound;
          p1 = (x1, y1);
          p2 = (x2, y2);
          if x1 <= x2 && y1 >= y2 && p1 != (0, 0) && p2 != (0, 0) && p1 != p2 && check(p1, p2)
        ) yield(p1, p2)
    println(r.size)
  }
}