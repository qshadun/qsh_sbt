package projecteuler

object Problem150 {
  val modulo = math.pow(2, 20).toInt
  val minus = modulo / 2
  var t = 0
  val s = Array.fill(500500 + 1)(0)
  1 to 500500 foreach {i =>
    t = ((615949L * t + 797807) % modulo).toInt
    s(i) = t - minus
  }
  
  val toLine = Array.fill(500500 + 1)(0)
  val prefixLineSum = Array.fill(500500 + 1)(0)
  var sum = 0
  1 to 1000 foreach{i =>
    sum + 1 to sum + i foreach{k =>
      toLine(k) = i
    }
    (1 to i).scanRight(0){(k, acc) =>
      val preSum = s(sum + k) + acc
      prefixLineSum(sum + k) = preSum
      preSum
    } 
    sum = sum + i
  }
  
  def allTriangleBounds(n: Int): List[(Int, Int)] = {
    val line = toLine(n)
    if (line == 1000) Nil
    else {
      def recur(sofar: List[(Int, Int)], l: Int, width: Int): List[(Int, Int)] =
        if (l == 1000) sofar.reverse
        else {
          val left = sofar.head._1
          recur((left + l, left + l + width) :: sofar, l + 1, width + 1)
        }
      recur(List((n + line, n+ line + 1)), line + 1, 2)
    }
  }
  
  def minFromOnePoint(n: Int) = allTriangleBounds(n).scanLeft(s(n)) {
//    case(acc, (left, right)) => acc + (left to right).map(s).sum
    case(acc, (left, right)) => acc + prefixLineSum(left) - prefixLineSum(right) + s(right)
  }.min
  def main(args: Array[String]): Unit = {
//    assert(s(1) == 273519)
//    assert(s(2) == -153582)
//    assert(s(3) == 450905)
//    assert(toLine(1) == 1)
//    assert(toLine(2) == 2)
//    assert(toLine(5) == 3)
//    assert(toLine(500500) == 1000)
//    assert(toLine(499490) == 999)
//    println(allTriangleBounds(1))
//    println(allTriangleBounds(500412))
//    println(allTriangleBounds(2))
    val min = (1 to 500500).par.map(minFromOnePoint).min
    println(min)
  }
}