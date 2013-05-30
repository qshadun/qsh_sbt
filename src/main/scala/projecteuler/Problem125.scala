package projecteuler
//Palindromic sums
object Problem125 {
  def isPalindromic(n: Int) = n.toString == n.toString.reverse
  val limit = 100000000
  val sqs = (1 to math.sqrt(limit / 2).toInt).map(i => i * i)
  def findSqPals(n: Int) = {
    var acc = List(sqs(n - 1))
    import scala.util.control.Breaks._
    breakable {
      sqs.drop(n).foreach { sq =>
        val t = acc.head + sq
        if (t > limit) break
        else acc = t +: acc
      }
    }
    acc.init.filter(isPalindromic)
  }
  def sumPals = (1 to math.sqrt(limit / 2).toInt).map(findSqPals).flatten.distinct.foldLeft(0L)(_ + _)
  def main(args: Array[String]): Unit = {
    println(sumPals)
  }
}