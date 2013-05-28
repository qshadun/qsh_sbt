package projecteuler
//Pandigital prime sets
object Problem118 {
//  import scala.collection.mutable
//  val pm = mutable.Map.empty[Int, List[Int]]
//  initPrimeMap
//  def initPrimeMap = {
//    (1 to 9).foreach { i =>
//      pm(i) = Nil
//    }
//    val sieve = Sieve(math.pow(10, 8).toInt) //no need to calculate 9 digits since 1+2+...+9 = 45, 45 % 3 == 0
//    sieve.getPrimes.foreach { i =>
//      val s = i.toString
//      if (s.indexOf("0") == -1 && s.distinct.size == s.size)
//        pm(s.size) = i :: pm(s.size)
//    }
//  }
//  def panSets(digits: List[Char]) = {
//    def recur(ds: List[Char], sofar: List[Int]): Set[Set[Int]] =
//      if (ds.isEmpty) Set(sofar.toSet)
//      else {
//        val (lastNumber, nd) = if (sofar.isEmpty) (Integer.MAX_VALUE, 9) else (sofar.head, sofar.head.toString.size)
//        val r = (nd - 1 to 1 by -1).map { i =>
//          pm(i).filter(n => n.toString.forall(ds.contains(_))).map { n =>
//            recur(ds.filterNot(n.toString.contains(_)), n +: sofar)
//          }.flatten
//        }.flatten.toSet
//        val s = pm(nd).filter(_ < lastNumber).filter(n => n.toString.forall(ds.contains(_))).map { n =>
//          recur(ds.filterNot(n.toString.contains(_)), n +: sofar)
//        }.flatten.toSet
//        r ++ s
//      }
//    recur(digits, Nil)
//  }
//  def main(args: Array[String]): Unit = {
//    val r = panSets((1 to 9).toList.map(i => (i + '0').toChar))
//    println(r.size)
//  }

  val sieve = Sieve(math.pow(10, 8).toInt)
  def isPrime = sieve.isPrime _
  def count(digits: List[Int]) = {
    def recur(ds: List[Int], pre: Int): Int =
      if (ds.isEmpty) 1
      else {
        (pre.toString.size to math.min(8, ds.size)).map { i =>
          val n = ds.take(i).mkString.toInt
          if (n > pre && isPrime(n)) recur(ds.drop(i), n)
          else 0
        }.sum
      }
    digits.permutations.map(recur(_, 0)).sum
  }
  def main(args: Array[String]) = {
    println(count((1 to 9).toList))
  }
}