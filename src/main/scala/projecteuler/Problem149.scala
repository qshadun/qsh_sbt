package projecteuler

import java.io.PrintWriter
import java.io.File

object Problem149 {
  val nums = Array.fill(4000000 + 1)(0)
  1 to 55 foreach { i =>
    nums(i) = ((100003 - 200003 * i + 300007L * i * i * i) % 1000000 - 500000).toInt
  }

  56 to 4000000 foreach { i =>
    nums(i) = (nums(i - 24) + nums(i - 55) + 1000000) % 1000000 - 500000
  }
  val table = nums.tail.grouped(2000).toArray

  def printNumTable = {
    assert(nums(10) == -393027)
    assert(nums(100) == 86613)
    val pw = new PrintWriter(new File("148_numbers.txt"))
    table.foreach { row =>
      pw.println(row.map(n => f"$n%7d").mkString(" "))
    }
    pw.close
  }

  def findMaxInOneDirection(i: Int, j: Int, dx: Int, dy: Int) = {
    var max = table(i)(j)
    var sum = table(i)(j)
    var x = i + dx
    var y = j + dy
    while (sum > 0 && inBound(x, y)) {
      sum = sum + table(x)(y)
      if (sum > max) max = sum
      x = x + dx
      y = y + dy
    }
    max
  }

  val directions = List((1, 0), (-1, 0), (0, 1), (0, -1), (1, 1), (-1, -1), (1, -1), (-1, 1))

  def inBound(x: Int, y: Int) = x >= 0 && x < 2000 && y >= 0 && y < 2000
  def findMax = {
    var max = 0
    for (i <- 0 until 2000; j <- 0 until 2000) {
      if (table(i)(j) > 0) {
        directions.filter {
          case (dx, dy) =>
            val adjX = i - dx
            val adjY = i - dy // adjacent in the opposite direction should be negative
            !inBound(adjX, adjY) || table(adjX)(adjY) <= 0
        }.foreach {
          case (dx, dy) =>
            val directMax = findMaxInOneDirection(i, j, dx, dy)
            if (directMax > max) max = directMax
        }
      }
    }
    max
  }
  def main(args: Array[String]): Unit = {
    //    printNumTable
    println(findMax)
  }
}