package projecteuler
/**
Reciprocal cycles
Problem 26
A unit fraction contains 1 in the numerator. The decimal representation of the unit fractions with denominators 2 to 10 are given:

1/2 =   0.5
1/3 =   0.(3)
1/4 =   0.25
1/5 =   0.2
1/6 =   0.1(6)
1/7 =   0.(142857)
1/8 =   0.125
1/9 =   0.(1)
1/10  =   0.1
Where 0.1(6) means 0.166666..., and has a 1-digit recurring cycle. It can be seen that 1/7 has a 6-digit recurring cycle.

Find the value of d  1000 for which 1/d contains the longest recurring cycle in its decimal fraction part.
 */
object Problem26 {
  import scala.collection.mutable.ArrayBuffer
  def divide(n: Int, d: Int) = {
    var remains = ArrayBuffer(n)
    var quotients = ArrayBuffer.empty[Int]
    var divided = false
    var cycleFound = false
    var cycle = List.empty[Int]
    while(! divided && !cycleFound) {
      val (q, r) = (remains.last / d, remains.last % d)
      quotients.append(q)
      if (r == 0){
        divided = true
      }
      else {
        val i = remains.indexOf(r * 10)
        if (i != -1 ) {
          cycleFound = true
          cycle = quotients.slice(i, quotients.length).toList
        } else {
          remains.append(r * 10)
        }
      }
    }
    (quotients.toList, cycle)
  }
  def main(args: Array[String]): Unit = {
    val r = (1 until 1000).map(divide(1, _)._2).zip(1 until 1000).foldLeft((List.empty[Int], 1)) {(acc, x) =>
      if (x._1.size > acc._1.size) x
      else acc
    }
    println(r)
  }

}