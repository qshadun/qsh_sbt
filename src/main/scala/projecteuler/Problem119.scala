package projecteuler
//Digit power sum
object Problem119 {
  def check(n: BigInt): Boolean = {
    val sum = BigInt(n.toString.map(_ - '0').sum)
    sum > 1 && {
      var i = 1
      while (sum.pow(i) < n) i = i + 1
      sum.pow(i) == n
    }
  }
  def main(args: Array[String]): Unit = {
    val digits = 15
    import scala.collection.mutable
    val limit = BigInt(10).pow(digits)
    val candidates = (2 to 9 * digits).map(BigInt.apply).map{n =>
      var i = 2
      var ab = mutable.ArrayBuffer.empty[BigInt]
      while(n.pow(i) < limit) {
        ab.append(n.pow(i))
        i = i + 1
      }
      ab.filter(_ > 10).toList
    }.flatten.distinct.sorted
    val result = candidates.filter(check).take(30) 
    if (result.size == 30)
      println(result.last)
  }
}