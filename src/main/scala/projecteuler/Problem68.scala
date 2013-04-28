package projecteuler
object Problem68{
  def main(args: Array[String]) = {
    def check(outside: Seq[Int], inside: Seq[Int]) = {
      val c = outside(0) + inside(0) + inside(1)
      (1 to 4).forall{i => 
        outside(i) + inside(i) + inside((i + 1) % 5) == c
      }
    }
    def toNumberRepr(outside: Seq[Int], inside: Seq[Int]) = {
      val start = outside.indexOf(outside.min)
      BigInt((0 to 4).map{i => 
        val lt = (start + i) % 5
        val gt = (start + i + 1) % 5
        List(outside(lt), inside(lt), inside(gt))
      }.map(_.mkString).mkString)
    }
    val ns = (1 to 9).toList
    val result = ns.combinations(5).map{inside => 
      val outside = ns.filterNot(inside.contains) :+ 10
      for (
        as <- inside.permutations;
        bs <- outside.permutations;
        if check(bs, as)
      ) yield((bs, as))
    }.flatten.map(r => toNumberRepr(r._1, r._2)).max
    println(result)
  }
}