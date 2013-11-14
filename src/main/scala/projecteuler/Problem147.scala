package projecteuler

object Problem147 {
  val wLimit = 47
  val hLimit = 43
  val counts = Array.fill(hLimit + 1)(Array.fill(wLimit + 1)(0)) 
  def countSquare(n: Int) = {
    val result = Array.fill(n + 1)(Array.fill(n + 1)(0))
    for (i <- 1 to n; j <- 1 to i) {
      result(i)(j) = (n +1 - i) * (n + 1 - j) 
      //if (i != j) result(j)(i) = result(i)(j)
    }
    result
  }
  
  def countSquareCrossHatched(n: Int) = {
   val result = Array.fill(2 * n - 1)(Array.fill(n/2 + 1)(0))
   for (i <- 1 to 2 * n - 2; j <- 1 to math.min(i, 2 * n - i)) {
     
   }
  }
  def main(args: Array[String]): Unit = {
    
  }
}