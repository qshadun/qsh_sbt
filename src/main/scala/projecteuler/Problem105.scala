package projecteuler

object Problem105 {
  def test(s: Array[Int]): Boolean = {
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
  def main(args: Array[String]): Unit = {
    import scala.io.Source
    val input = Source.fromURL(getClass.getResource("/sets.txt")).getLines.map(_.split(',').map(_.toInt))
    println(input.filter(test).map(_.sum).sum)
  }
}