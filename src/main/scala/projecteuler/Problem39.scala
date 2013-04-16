package projecteuler
/**
Integer right triangles
Problem 39
If p is the perimeter of a right angle triangle with integral length sides, {a,b,c}, 
there are exactly three solutions for p = 120.

{20,48,52}, {24,45,51}, {30,40,50}

For which value of p <= 1000, is the number of solutions maximised?
 */
object Problem39 {
  
  
  def main(args: Array[String]) {
//    // a + b > c, therefore c < 500
//    val squares = Stream.from(1).map(x => x * x).takeWhile(_ < 500 * 500).toSet
    def isSquare(n: Int) = {
      val root = Math.sqrt(n).toInt
      root * root == n
    }
    
    import scala.collection.mutable
    val records = mutable.Map.empty[Int, List[(Int, Int, Int)]]
    for (
      a <- 1 to 333; 
      b <- a until 500;
      cSquare = a * a + b * b;
      if isSquare(cSquare);
      c = Math.sqrt(cSquare).toInt;
      p = a + b + c if a + b + c <= 1000
    ) {
      if (records.contains(p)) records(p) = (a, b, c) :: records(p);
      else records(p) = List((a, b, c))
    }
    println(records(120))
    println(records.maxBy(_._2.size))
  }
}