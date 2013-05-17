package projecteuler
//Special subset sums: testing
object Problem105 {
  import Problem106._
  def naiveTest(s: Array[Int]): Boolean = {
    val subs = (1 to s.size).map(s.combinations(_)).flatten.toArray
    (0 to subs.size / 2).forall { i =>
      (i + 1 to subs.size - 1).forall { j =>
        if (subs(i).forall(!subs(j).contains(_))) {
          val s1 = subs(i).sum
          val s2 = subs(j).sum
          s1 != s2 &&
            (subs(i).size < subs(j).size && subs(i).sum < subs(j).sum ||
              subs(i).size == subs(j).size)
        } else true
      }
    }
  }
  import scala.collection.mutable
  val pairs = mutable.Map.empty[Int, Seq[(List[Int], List[Int])]]
  def getPairs(n: Int) = 
    if (pairs.contains(n)) pairs(n)
    else {
      val p = pairsNeedCheck(n)
      pairs(n) = p
      p
    }
  def test(s: Array[Int]): Boolean = {
    val sorted = s.sorted
    val largestSubSetSize = if (s.size % 2 == 0) s.size / 2 else s.size / 2  + 1
    s.distinct.size == s.size &&
    (2 to largestSubSetSize).forall{x =>
      sorted.slice(0, x).sum > sorted.slice(s.size - x + 1, s.size).sum
    } &&
    getPairs(sorted.size).forall{case(p1, p2) =>
      p1.map(sorted.apply).sum != p2.map(sorted.apply).sum
    }
  }
  def main(args: Array[String]): Unit = {
    import scala.io.Source
    val input = Source.fromURL(getClass.getResource("/sets.txt")).getLines.map(_.split(',').map(_.toInt))
    println(input.filter(test).map(_.sum).sum)
  }
}