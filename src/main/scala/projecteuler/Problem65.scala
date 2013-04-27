package projecteuler
object Problem65 {
  def main(args: Array[String]) = {
    val seq = 2 :: Stream.from(1).map{x =>
      if (x % 3 != 2) 1 else 2 * (x + 1) /3 
    }.take(99).toList
    val cov = seq.init.foldRight(BigInt(seq.last), (BigInt(1))){(x, acc) =>
      (acc._1 * x + acc._2, acc._1)
    }
    println(cov._1.toString.map(_ - '0').sum)
  }
}