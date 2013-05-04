package projecteuler
object Problem82{
  def main(args: Array[String]) = {
   val input = scala.io.Source.fromURL(getClass.getResource("/matrix.txt")).getLines.map{line =>
     line.trim.split(',').map(_.toInt).toArray
   }.toArray
    object Move extends Enumeration {
      type Move = Value
      val Right, Up, Down = Value
    }
    import Move._
    import scala.collection.mutable
    def solve(matrix: Array[Array[Int]]): Int = {
      def loop(pos: mutable.Map[(Int, Int, Move), Int], min: Int): Int = {
        if (pos.isEmpty) min
        else {
          val m = mutable.Map.empty[(Int, Int, Move), Int]
          var nextMin = min
          pos.filter(_._2 < min).map {case ((x, y, pre), sum) =>
            if (y < matrix.size - 1) {
              if (x > 0 && pre != Down) { // can go up
                val sU = sum + matrix(x - 1)(y)
                val pU = (x - 1, y, Up)
                if (m.contains(pU)) m(pU) = math.min(sU, m(pU))
                else m(pU) = sU
              }
              if (x < matrix.size -1 && pre != Up) { // can go down
                val sD = sum + matrix(x + 1)(y)
                val pD = (x + 1, y, Down)
                if (m.contains(pD)) m(pD) = math.min(sD, m(pD))
                else m(pD) = sD
              }
              // go right
              val sR = sum + matrix(x)(y + 1)
              val pR = (x, y + 1, Right)
              if (m.contains(pR)) m(pR) = math.min(sR, m(pR))
              else m(pR) = sR
            } else {
              nextMin = math.min(nextMin, sum)
            }
          }
          loop(m, nextMin)
        }
      }
      val start =  mutable.Map.empty[(Int, Int, Move), Int]
      (0 until matrix.size).foreach{x =>
        start((x, 0, Right)) = matrix(x)(0)
      }
      loop(start, Int.MaxValue)
    }
    println(solve(input))
  }
}
