package projecteuler

import java.io.PrintWriter
import java.io.File

object Problem148 {
  def generateTriangle(n: Int) = {
    val triangle = Array.fill(n)(Array(1))
    1 to n-1 foreach{i =>
      triangle(i) = Array.fill(i+1)(1)
      1 to i-1 foreach {j =>
        triangle(i)(j) = (triangle(i-1)(j-1) + triangle(i-1)(j)) % 7 
      }
    }
    triangle.map(row => " " * (n - row.size) + row.mkString(" ")).mkString("\n")
  }
  
  def solve(limit: Long) = {
    val m = scala.collection.mutable.Map(1L -> 1L)
    Stream.iterate(7L)(_ * 7L).takeWhile(_ <= limit).foreach {i =>
      m(i) = 28 * m(i / 7)
    }
    println(m)
    def decompose(remain: Long, divider: Long): List[(Long, Long)] = 
      if (remain == 0) Nil
      else {
        val s = remain / divider
        val newRemain = remain % divider
        (s, divider) :: decompose(newRemain, divider/7)
      }
    val deComposed = decompose(limit, m.keys.max)
    println(deComposed)
    
    def compute(parts: List[(Long, Long)], multiple: Long): Long = parts match {
      case Nil => 0
      case (s, d) :: remain =>
        val times: Long = 1 to s.toInt sum
        val t = times * m(d) * multiple
        t + compute(remain, multiple * (s + 1))
    } 
    println(compute(deComposed, 1))
  }
  def main(args: Array[String]): Unit = {
//    val triangle = generateTriangle(500)
//    val pw = new PrintWriter(new File("triangle.txt"))
//    pw.print(triangle)
//    pw.close
    solve(1000 * 1000 * 1000)
//    solve(100)
  }
}