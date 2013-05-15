package projecteuler

object Problem102 {
  // y = k*x + d
  def calcLine(p1: (Double, Double), p2: (Double, Double)): (Double, Double) = {
    val (x1, y1) = p1
    val (x2, y2) = p2
    ((y1 - y2) / (x1 - x2), (x1 * y2 - x2 * y1) / (x1 - x2))
  }
  // find the intersection point of two line
  def findInterPoint(l1: (Double, Double), l2: (Double, Double)): (Double, Double) =
    {
      val (k1, d1) = l1
      val (k2, d2) = l2
      val x = (d2 - d1) / (k1 - k2)
      val y = (k1 * d2 - k2 * d1) / (k1 - k2)
      (x, y)
    }
  def containOrigin(p1: (Double, Double), p2: (Double, Double), p3: (Double, Double)): Boolean =
    if (List(p1, p2, p3).contains((0D, 0D))) true
    else {
      val (k1, d1) = calcLine((0, 0), p1)
      val (k2, d2) = calcLine(p2, p3)
      if (k1 == k2) false
      else {
        val (x, y)= findInterPoint((k1, d1), (k2, d2))
        val xs = List(p2, p3).map(_._1).sorted
        val ys = List(p2, p3).map(_._2).sorted
        val (x1 , y1) = p1
        x > xs(0) && x < xs(1) && y > ys(0) && y < ys(1) &&
        x1 * x <= 0 && y1 * y <= 0 
      }
    }
  def containOrigin(ps: Seq[(Double, Double)]): Boolean = containOrigin(ps(0), ps(1), ps(2))
  def main(args: Array[String]): Unit = {
    import scala.io.Source
    val input = Source.fromURL(getClass.getResource("/triangles.txt")).getLines.map{line =>
      line.split(',').map(_.toDouble).grouped(2).map(ls => (ls(0), ls(1))).toList
    }
    val result = input.map(ps => (ps, containOrigin(ps))).toList
    result.foreach(println)
    println(result.filter(_._2).size)
  }
}