package projecteuler
/**
 * http://www.mathblog.dk/project-euler-144-investigating-multiple-reflections-of-a-laser-beam/
 */
object Problem144 {
  def next(pa: (Double, Double), po: (Double, Double)) = {
    val m0 = -4 * po._1 / po._2
    val m1 = (po._2 - pa._2) / (po._1 - pa._1)
    val tanA = (m1 - m0)/(1 + m0 * m1)
    val k = (m0 - tanA) / (1 + tanA * m0)
    val d = po._2 - k * po._1
    val a = k * k + 4
    val b = 2 * k * d
    val c = d * d - 100
    val rPart = math.sqrt(b * b - 4 * a * c)
    val x1 = (-b + rPart) / (2 * a)
    val x2 = (-b - rPart) / (2 * a) 
    val x = if (math.abs(x1 - po._1) > math.abs(x2 - po._1)) x1 else x2 
    val y = k * x + d
    (x, y)
  }
  def main(args: Array[String]): Unit = {
    var count = 0
    var p1 = (0D, 10.1)
    var p2 = (1.4, -9.6)
    while(! (p2._1 >= -0.01 && p2._1 <= 0.01 && p2._2 > 0)) {
      count = count + 1
      val p3 = next(p1, p2)
      println(p3)
      p1 = p2
      p2 = p3
    }
    println(count)
//    println(next(p1, p2))
  }

}