package projecteuler
/**
http://www.mathblog.dk/project-euler-142-perfect-square-collection/
a = x + y
b = x - y
c = x + z
d = x - z
e = y + z
f = y - z

f = y - z = (x + y) - (x + z) = a - c
e = y + z = (x + y) - (x - z) = a - d
b = x - y = (x + z) - (y + z) = c - e = c - (a - d) = c + d - a

x = (a + b)/2
y = (e + f)/2
z = (c - d)/2
 */
object Problem142 {
  def isPerfectSquare(n: Long) = {
    val root = math.sqrt(n).toLong
    root * root == n
  }
//  def pairs(limit: Int) = {
//    for (
//      x <- 2 to limit;
//      y <- 1 until x;
//      if (isPerfectSquare(x - y) && isPerfectSquare(x + y))
//    ) yield (x, y)
//  }
//
//  def find = {
//    val pm = pairs(100000).groupBy(_._2).map {
//      case (x, xs) => (x, xs.map(_._1).toSet)
//    }
//    val result = pm.keys.toList.sorted.map { z =>
//      val candidates = pm(z)
//      if (candidates.size > 1) {
//        val xys = candidates.init.flatMap { y =>
//          candidates.filter(_ > y).map(x => (x, y)).filter {
//            case (x, y) => pm.contains(y) && pm(y).contains(x)
//          }
//        }
//        xys.map {
//          case (x, y) => (x, y, z)
//        }
//      } else Set.empty[(Int, Int, Int)]
//    }.flatten
//    println(result)
//    if (!result.isEmpty) {
//      println(result.map {
//        case (x, y, z) => x + y + z
//      }.min)
//    }
//  }
  def main(args: Array[String]): Unit = {
//    println(find)
    var find = false
    var i: Long = 3
    while(!find) {
      val a = i * i
      var j: Long = 2
      while(!find && j < i) {
        val c = j * j
        var k: Long = if (j % 2 == 0) 2 else 1 //c and d must be same parity because (c - d) can be divide by 2  
        while(! find && k < j) {
          val d = k * k
          if (c + d > a && isPerfectSquare(c + d - a) && isPerfectSquare(a - c) && isPerfectSquare(a - d)) {
            find = true;
            val x = (c + d)/2
            val y = (2* a - c - d)/2
            val z = (c - d)/2
            println(s"$x, $y, $z, ${x + y + z}")
          }
          k = k + 2
        }
        j = j + 1
      }
      i = i + 1
    }
  }

}