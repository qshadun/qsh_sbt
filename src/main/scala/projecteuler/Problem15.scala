package projecteuler
/**
Lattice paths
Problem 15
Starting in the top left corner of a 2*2 grid, and only being able to move to the right and down, there are exactly 6 routes to the bottom right corner.


How many such routes are there through a 20*20 grid?
 */
object Problem15 {
  def combination(total: Int, n: Int) = {
    require(total > n)
    require(n > 0)
    ((n + 1) to total).foldLeft(BigInt(1))(_ * _) /
    (1 to n).foldLeft(BigInt(1))(_ * _)
  }
  
  def main(args: Array[String]): Unit = {
    def paths(n: Int) = {
      def pathR(soFar: (Int, Int)): Long = {
        val x = soFar._1
        val y = soFar._2
        if (x + 1 <= n && y + 1 <= n)
          pathR((x+1, y)) + pathR((x, y+1))
        else if (x + 1 <= n)
          pathR((x+1, y))
        else if (y + 1 <= n)
          pathR((x, y+1))
        else
          1L
      }
      pathR((0, 0))
    }
    
    
    println(paths(20))
    println(combination(40, 20))
  }

}