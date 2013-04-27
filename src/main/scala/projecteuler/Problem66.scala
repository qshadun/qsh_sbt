package projecteuler
object Problem66 {
  def main(args: Array[String]) = {
    val squares = (1 to 10000000).map(x => 1L * x * x)
    val ds = (2 to 1000).toList -- squares.takeWhile(_ <= 1000).map(_.toInt).toList
    def isPerfectSquare(n: BigInt) = {
      val root = BigDecimal(math.sqrt(n.toDouble)).toBigInt
      root * root == n
    }
//  val result = ds.map{d =>
//    val ySquare = squares.find{y => 
//      val xSquare = 1 + d * y 
//      isPerfectSquare(xSquare)
//    }
//    if (ySquare.isDefined) {
//      val r = (d, ySquare.get, 1 + d * ySquare.get)
//      //println(r)
//      r
//    } else {
//      println("no result for " + d)
//      (d, 0L, 0L)
//    }
//    
//  }
//  println(result.maxBy(_._3))
// (x + 1) (x - 1) = D * y * y, list the sequence of (x + 1)(x - 1), each x is the minimum for which D until all D under 1000 are exhausted
  def factorials(n: BigInt): List[(BigInt, Int)] = {
    def recur(f: BigInt, r: BigInt, fs: List[(BigInt, Int)]): List[(BigInt, Int)] = {
      if (r == 1) fs.sortBy(_._1)
      else 
        if (r % f == 0)
          if (fs.size > 0 && fs.head._1 == f) recur(f, r/f, (fs.head._1, fs.head._2 + 1) :: fs.tail)
          else recur(f, r/f, (f, 1) :: fs)
        else recur(f+1, r, fs)
    }
    recur(2, n, Nil)
  }
  def combine(fs1: List[(BigInt, Int)], fs2: List[(BigInt, Int)]) = 
    (fs1 ::: fs2).groupBy(_._1).map(x => if (x._2.size ==1) x._2.head else (x._1, x._2.head._2 + x._2.last._2)).toList
  def check(x1: BigInt, x2: BigInt, d: Int) = x1 * x2 % d == 0 && isPerfectSquare(x1 * x2 / d)
  def findX(d: Int) = {
    val factors = factorials(d)
    val largestFactor = factors.last._1
    var finded = false
    var x = BigInt(0)
    var t = BigInt(1)
    while (!finded) {
      // (x + 1)(x - 1), one number can divide d, the qua times another number is a square
      val x1 = largestFactor * t
      if (x1 > 2 && check(x1, x1 - 2, d)) {
        finded = true
        x = x1 - 1
      } else if (check(x1, x1 + 2, d)) {
        finded = true
        x = x1 + 1
      }
      if (!finded) t = t + 1
    }
    println(d + ", " + x)
    x
  }
  val result = ds.map(findX)
  println(result)
  println(result.max)
//  def possibleD(fs: List[(Int, Int)]): List[Int] = {
//    def pow(a: Int, b: Int):Int = if (b == 0) 1 else a * pow(a, b - 1)
//    def toD(facs: List[(Int, Int)]) = facs.foldLeft(1){(acc, x) =>
//      acc * pow(x._1, x._2)
//    }
//    toD(fs) :: fs.map{x =>
//      if (x._2 >= 2) {
//        val newFs = if (x._2 == 2) fs - x else (x._1, x._2 - 2) :: (fs - x)
//        possibleD(newFs)
//      }
//      else Nil
//    }.flatten 
//  }
//    import scala.util.control.Breaks._
//  breakable {
//    Stream.from(2).foldLeft(ds){(acc, x) =>
//      if (x % 1000 == 0) println(x + ", " + acc)
//      val d = possibleD(combine(factorials(x - 1), factorials(x + 1)))
//      val nextAcc = acc -- d
//      if (nextAcc.size == 1) {
//        println(acc, x)
//        break
//      }
//      nextAcc
//    }
//  }
  }
}