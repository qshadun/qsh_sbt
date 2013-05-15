package projecteuler
//Optimum polynomial
object Problem101 {
  def toSequence(ces: Array[BigInt]): Stream[BigInt] =
    Stream.from(1).map { i =>
      ces.zipWithIndex.foldLeft(BigInt(0)) { (acc, a) =>
        acc + a._1 * BigInt(i).pow(a._2)
      }
    }
  def solveMatrix(matrix: Array[Array[BigInt]]): Array[Rational] =
    if (matrix.length == 1) Array(Rational(matrix(0)(1), matrix(0)(0)).normalize)
    else {
      val m = matrix.map(_.map(Rational.apply))
      def swapRow(i: Int, j: Int) = {
        val temp = m(i)
        m(i) = m(j)
        m(j) = temp
      }
      for (i <- 0 to m.length - 1) {
        if (m(i)(i).n == 0) {
          val j = (i + 1 to m.length - 1).find(m(_)(i).n != 0).get
          swapRow(i, j)
        }
        //make dialog 1
        for (j <- i + 1 to m.length)
          m(i)(j) = (m(i)(j) / m(i)(i)).normalize
        m(i)(i) = 1

        for (j <- i + 1 to m.length - 1) {
          for (k <- i + 1 to m.length) {
            m(j)(k) = m(j)(k) - m(j)(i) * m(i)(k)
          }
          m(j)(i) = 0
        }
        //      printMatrix(m)
      }
      for (i <- m.length - 2 to 0 by -1) {
        for (j <- m.length - 1 to i + 1 by -1) {
          m(i)(m.length) = m(i)(m.length) - m(i)(j) * m(j)(m.length)
          m(i)(j) = 0
        }
        //      printMatrix(m)
      }
      m.map(_.last.normalize)
    }
  def printMatrix(matrix: Array[Array[Rational]]) = {
    matrix.map(_.toList).foreach(println(_))
    println
  }
  def findCoefficients(matrix: Array[Array[BigInt]]) = {
    val r = solveMatrix(matrix)
    require(r.forall(_.d == 1))
    r.map(_.n)
  }
  def sumFit(ce: Array[BigInt]) = {
    def seq = toSequence(ce)
    val fits = (1 to ce.length - 1).map { n =>
      val matrix = Array.tabulate(n)(_ + 1).map { i =>
        Array.tabulate(n)(identity).map { BigInt(i).pow(_) } :+ seq(i - 1)
      }
      val op = findCoefficients(matrix)
      println(op.toList)
      val opSeq = toSequence(op)
      val fitIndex = Stream.from(0).find(i => seq(i) != opSeq(i)).get
      opSeq(fitIndex)
    }
    println(fits)
    fits.sum
  }
  def main(args: Array[String]): Unit = {
    //    val seq = toSequence(Stream.continually(List(1, -1)).flatten.map(BigInt.apply).take(11).toArray) 
    //    println(seq.take(20).toList)
    //    val seq1 = toSequence(Array(0, 0, 0, 1).map(BigInt.apply))
    //    println(seq1.take(10).toList)
    //    val m = Array(
    //          Array(1, 3, -2, 5),
    //          Array(3, 5, 6, 7),
    //          Array(2, 4, 3, 8)
    //        ).map(_.map(BigInt.apply))
    //    println(solveMatrix(m).toList)
    println(sumFit(Stream.continually(List(1, -1)).flatten.map(BigInt.apply).take(11).toArray))
  }
}