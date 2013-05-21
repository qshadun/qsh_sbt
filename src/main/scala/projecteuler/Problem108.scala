package projecteuler
//Diophantine reciprocals I
/**
let x = n + a, y = n + b, then n * n = a * b
 */
object Problem108 {
  def countSolutions(factors: List[(Int, Int)]) = {
    val n = toNumber(factors)
    val fs = factors.map{case(f, n) => List.fill(2 * n)(f)}.flatten.map(BigInt.apply)
    (1 to fs.size).map{i =>
      fs.combinations(i).filter(_.product <= n).size
    }.sum + 1
  }
  def toNumber(factors: List[(Int, Int)]) = factors.foldLeft(BigInt(1)){case (acc, (f, n)) => BigInt(f).pow(n) * acc}  
  def main(args: Array[String]): Unit = {
    val fs = List((2, 2), (3, 2), (5, 1), (7, 1), (11, 1), (13, 1))
    println(countSolutions(fs))
    println(toNumber(fs))
  }
}