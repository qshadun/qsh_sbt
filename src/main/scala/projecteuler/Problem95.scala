package projecteuler
// Amicable chains
object Problem95 {
//  def divisorsSum(n: Int): Int = 
//    (2 to math.sqrt(n).toInt).foldLeft(1){(acc, x) =>
//      if (n % x == 0) acc + x + n / x
//      else acc
//    }
  def divisorsSum(n: Int) = divSum(n)
  val bound = 1000000
  val ar = Array.fill(bound + 1)(0)
  val divSum = Array.fill(bound+1)(1)
  def initDivSum = {
    (2 to bound/2).foreach{factor =>
      (2*factor to bound by factor).foreach(x => divSum(x) = divSum(x) + factor)
    }
  }
  initDivSum
  def findChain(n: Int): Unit = {
    def recur(x: Int, seen: Seq[Int]): Unit = {
      val d = divisorsSum(x)
      if (d > bound || ar(d) != 0) seen.foreach(i => ar(i) = -1)
      else
        if (seen.contains(d)) {
          val c = seen.indexOf(d)
          seen.take(c + 1).foreach(i => ar(i) = c + 1)
          seen.drop(c + 1).foreach(i => ar(i) = -1)
        }
        else recur(d, d +: seen)
    }
    if (ar(n) == 0) {
      recur(n, List(n))
    }
  }
  def main(args: Array[String]): Unit = {
    (2 to bound).foreach(findChain)
    println(ar.indexOf(ar.max))
  }
}