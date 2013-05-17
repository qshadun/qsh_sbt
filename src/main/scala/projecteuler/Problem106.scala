package projecteuler
//Special subset sums: meta-testing
object Problem106 {
  def pairsNeedCheck(n: Int): Seq[(List[Int], List[Int])] = {
    val indexes = List.tabulate(n)(identity)
    (2 to n / 2).map { x =>
      findPairs(Nil, Nil, x, indexes, true)
    }.flatten
  }
  def findPairs(p1: List[Int], p2: List[Int], pairSize: Int, remains: List[Int], needReverse: Boolean): List[(List[Int], List[Int])] =
    if (p1.size == pairSize) {
      if (needReverse) Nil
      else List((p1, p2))
    } else {
      if (remains.size < (pairSize - p1.size) * 2) Nil
      else {
        val f1 = if (p1.isEmpty) -1 else p1.head
        val f2 = if (p1.isEmpty) -1 else p2.head
        val p = for (
          i <- remains.filter(_ > f1);
          j <- remains.filter(_ > f2);
          if p1.isEmpty && i < j || !p1.isEmpty && i != j
        ) yield {
          val newRemains = remains.filter { x =>
            x != i && x != j && x > math.min(i, j)
          }
          val newNeedReverse = needReverse && i < j
          findPairs(i :: p1, j :: p2, pairSize, newRemains, newNeedReverse)
        }
        p.flatten
      }
    }
  def main(args: Array[String]): Unit = {
    println(List(4, 7, 12).map(pairsNeedCheck(_).size))
  }
}