package projecteuler
//Minimal network
object Problem107 {
  // Prim's algrorithm
  def mst(g: Array[Array[Int]]): Seq[(Int, Int)] = {
    import scala.collection.mutable
    val key = mutable.Map.empty[Int, Int] // the key function
    (1 until g.length).foreach(key(_) = Integer.MAX_VALUE)
    key(0) = 0
    val vq = mutable.Set.empty[Int] // vertexes in Q
    (0 until g.length).foreach(vq += _)
    val phi = mutable.Map.empty[Int, Int] // the phi function(parent function of the mst) 
    while (!vq.isEmpty) {
      val u = vq.minBy(key.apply)
      vq -= u
      (0 until g.length).filter(g(u)(_) > 0).map { v =>
        if (vq.contains(v) && g(u)(v) < key(v)) {
          phi(v) = u
          key(v) = g(u)(v)
        }
      }
    }
    phi.toSeq
  }
  def main(args: Array[String]): Unit = {
    //    val input = Array(
    //        Array(0,16,12,21,0,0,0),
    //        Array(16,0,0,17,20,0,0),
    //        Array(12,0,0,28,0,31,0),
    //        Array(21,17,28,0,18,19,23),
    //        Array(0,20,0,18,0,0,11),
    //        Array(0,0,31,19,0,0,27),
    //        Array(0,0,0,23,11,27,0))
    import scala.io.Source
    val input = Source.fromURL(getClass.getResource("/network.txt")).getLines.toArray.
      map(_.split(',').map { x =>
        if (x == "-") 0 else x.toInt
      })
    val result = mst(input)
    val total = input.map(_.sum).sum / 2
    val mstSum = result.map { case (u, v) => input(u)(v) }.sum
    println(total + " -  " + mstSum + " = " + (total - mstSum))
  }
}