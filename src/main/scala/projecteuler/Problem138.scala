package projecteuler
//Special isosceles triangles
/**
let b = 2 * k, we can get k = (sqrt(5 * L * L - 1) +/- 2) / 5
 */
object Problem138 {
  import ContinuedFraction._
  def isPerfectSquare(n: BigInt) = {
    val r = BigDecimal(math.sqrt(n.toDouble)).toBigInt
    r * r == n
  }
  def isosceles: Stream[BigInt] = 
    continuedFraction(5).map(_._2).drop(1).filter(l => isPerfectSquare(5 * l * l - 1))
  def main(args: Array[String]): Unit = {
    val result = isosceles.take(12)
    println(result.toList)
    println(result.sum)
  }
}