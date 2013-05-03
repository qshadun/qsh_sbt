package projecteuler
/**
Path sum: two ways
Problem 81
In the 5 by 5 matrix below, the minimal path sum from the top left to the bottom right, by only moving to the right and down, 
is indicated in bold red and is equal to 2427.


131	673	234	103	18
201	96	342	965	150
630	803	746	422	111
537	699	497	121	956
805	732	524	37	331

Find the minimal path sum, in matrix.txt (right click and 'Save Link/Target As...'), a 31K text file containing a 80 by 80 matrix, 
from the top left to the bottom right by only moving right and down.
*/
object Problem81{
  def main(args: Array[String]) = {
    val input = scala.io.Source.fromURL(getClass.getResource("/matrix.txt")).getLines.map{line =>
      line.trim.split(',').map(_.toInt).toArray
    }.toArray
    import scala.collection.mutable
    def solve(matrix: Array[Array[Int]]): Int = {
      def loop(pos: mutable.Map[(Int, Int), Int]): Int = {
        if (pos.keys.head == (matrix.size -1, matrix.size - 1)) pos.values.head
        else {
          val m = mutable.Map.empty[(Int, Int), Int]
          pos.map {case ((x, y), sum) =>
            if (x < matrix.size - 1) {
              val s = sum + matrix(x + 1)(y)
              val p = (x + 1, y)
              if (m.contains(p)) m(p) = math.min(s, m(p))
              else m(p) = s
            }
            if (y < matrix.size - 1) {
              val s = sum + matrix(x)(y + 1)
              val p = (x, y + 1)
              if (m.contains(p)) m(p) = math.min(s, m(p))
              else m(p) = s
            }
          }
          loop(m)
        }
      }
      loop(mutable.Map((0, 0) -> matrix(0)(0)))
    }
    println(solve(input))
  }
}
