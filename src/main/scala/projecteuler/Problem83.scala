package projecteuler
object Problem83{
  def main(args: Array[String]) = {
   val input = scala.io.Source.fromURL(getClass.getResource("/matrix.txt")).getLines.map{line =>
     line.trim.split(',').map(_.toInt).toArray
   }.toArray
    import scala.collection.mutable
    def solve(matrix: Array[Array[Int]]): Int = {
      def loop(pos: mutable.Map[(Int, Int), (Int, List[(Int, Int)])], min: Int): Int = {
        if (pos.isEmpty) min
        else {
          val m = mutable.Map.empty[(Int, Int), (Int, List[(Int, Int)])]
          var nextMin = min
          pos.filter(_._2._1 < min).map {case ((x, y), (sum, path)) =>
            if (y < matrix.size - 1 || x < matrix.size - 1) {
              if (x < matrix.size -1 && !path.contains((x + 1, y))) { // can go down
                val sD = sum + matrix(x + 1)(y)
                val pD = (x + 1, y)
                if (!m.contains(pD) || m(pD)._1 > sD) m(pD) = (sD, pD :: path)
              }
              if (x > 0 && !path.contains((x - 1, y))) { // can go up
                val sU = sum + matrix(x - 1)(y)
                val pU = (x - 1, y)
                if (!m.contains(pU) || m(pU)._1 > sU) m(pU) = (sU, pU :: path)
              }
              if (y < matrix.size - 1 && ! path.contains((x, y + 1))) { // go right 
                val sR = sum + matrix(x)(y + 1)
                val pR = (x, y + 1)
                if (!m.contains(pR) || m(pR)._1 > sR) m(pR) = (sR, pR :: path)
              }
              if (y > 0 && ! path.contains((x, y - 1))) { // go left
                val sL = sum + matrix(x)(y - 1)
                val pL = (x, y - 1)
                if (!m.contains(pL) || m(pL)._1 > sL) m(pL) = (sL, pL :: path)
              }
            } else {
              nextMin = math.min(nextMin, sum)
            }
          }
          loop(m, nextMin)
        }
      }
      loop(mutable.Map((0, 0) -> (matrix(0)(0), List((0, 0)))), Int.MaxValue)
    }
    println(solve(input))
  }
}
