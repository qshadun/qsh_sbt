package projecteuler

object Sudoku {
  case class SudokuResult(result: Seq[Seq[Int]]) extends Exception
  val numbers = List.tabulate(9)(_ + 1)
  def solve(puzzle: Seq[Seq[Int]]): Seq[Seq[Int]] = {
    def recur(pz: Seq[Seq[Int]]): Unit = {
      def indexGroup(n: Int): List[Int] =
        if (n < 3) List(0, 1, 2)
        else if (n < 6) List(3, 4, 5)
        else List(6, 7, 8)
      def possibleValues(x: Int, y: Int): List[Int] = {
        val usedInLine = pz(x).filterNot(_ == 0)
        val usedInColumn = (0 until 8).map(pz(_)(y)).filterNot(_ == 0)
        val usedInBox = (for (
          i <- indexGroup(x);
          j <- indexGroup(y)
        ) yield pz(i)(j)).filterNot(_ == 0)
        val used = usedInLine ++ usedInColumn ++ usedInBox
        numbers.filterNot(used.contains)
      }
      if (pz.forall(line => line.forall(_ != 0))) throw SudokuResult(pz)
      else {
        val ps = for (i <- 0 to 8; j <- 0 to 8; if pz(i)(j) == 0) yield ((i, j), possibleValues(i, j))
        val least = ps.sortBy(_._2.size).head
        if (least._2.isEmpty) return // no possible value
        else {
          val (x, y) = least._1
          least._2.foreach { v =>
            val newPz = (0 to 8).map { i =>
              if (i == x)
                (0 to 8).map { j =>
                  if (j == y) v else pz(i)(j)
                }
              else pz(i)
            }
            recur(newPz)
          }
        }
      }
    }
    try {
      recur(puzzle)
      Nil
    } catch {
      case e: SudokuResult => e.result
    }
  }

  def printResult(seq: Seq[Seq[Int]]) = {
    require(seq.size == 9)
    require(seq.forall(_.size == 9))
    def printSeparator = println(("-" * 19).mkString)
    def printOneLine(line: Seq[Int]) = {
      print("|")
      for (i <- 1 to 18) {
        if (i % 2 == 1) // odd, print number
          print(line(i / 2))
        else if (i % 6 == 0) print("|")
        else print(" ")
      }
      println
    }
    printSeparator
    for (i <- 1 to 12) {
      if (i % 4 == 0) printSeparator
      else printOneLine(seq(i - 1 - i / 4))
    }
  }
  def main(args: Array[String]): Unit = {
    import scala.io.Source
    val input = Source.fromURL(getClass.getResource("/sudoku.txt")).getLines.grouped(10).map { lines =>
      lines.tail.map(_.map(_ - '0'))
    }
    val solutions = input.map(solve).toList
    solutions.foreach(printResult)
    val numbers = solutions.map { su =>
      List(su(0)(0), su(0)(1), su(0)(2)).mkString.toInt
    }
    println(numbers)
    println(numbers.sum)
    //    val pz = List(
    //          List(0,0,3,0,2,0,6,0,0),
    //          List(9,0,0,3,0,5,0,0,1),
    //          List(0,0,1,8,0,6,4,0,0),
    //          List(0,0,8,1,0,2,9,0,0),
    //          List(7,0,0,0,0,0,0,0,8),
    //          List(0,0,6,7,0,8,2,0,0),
    //          List(0,0,2,6,0,9,5,0,0),
    //          List(8,0,0,2,0,3,0,0,9),
    //          List(0,0,5,0,1,0,3,0,0)
    //        )
    //    printResult(solve(pz))
  }
}