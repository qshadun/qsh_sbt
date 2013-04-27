package projecteuler
object Problem61 {
  def main(args: Array[String]) = {
    val triangles = Stream.from(1).map(n => n * (n + 1) /2).dropWhile(_ < 1000).takeWhile(_ < 10000).toList.map(_.toString)
    val squares = Stream.from(1).map(n => n * n).dropWhile(_ < 1000).takeWhile(_ < 10000).toList.map(_.toString)
    val pentagonals = Stream.from(1).map(n => n * (3*n - 1) /2).dropWhile(_ < 1000).takeWhile(_ < 10000).toList.map(_.toString)
    val hexgonals = Stream.from(1).map(n => n * (2*n - 1)).dropWhile(_ < 1000).takeWhile(_ < 10000).toList.map(_.toString)
    val heptagonals = Stream.from(1).map(n => n * (5*n - 3) /2).dropWhile(_ < 1000).takeWhile(_ < 10000).toList.map(_.toString)
    val octagonals = Stream.from(1).map(n => n * (3*n - 2)).dropWhile(_ < 1000).takeWhile(_ < 10000).toList.map(_.toString)
    def find(ns: List[List[String]]) = {
      def recur(sofar: List[String], candidates: List[List[String]]):List[List[String]] = {
        if (sofar.size == 6)
          if (sofar.head.substring(2,4) == sofar.last.substring(0,2)) List(sofar) else Nil
        else {
          candidates.filter(_.find(_.startsWith(sofar.head.substring(2,4))).isDefined).map{cs =>
            cs.filter(_.startsWith(sofar.head.substring(2,4))).map(x => recur(x :: sofar, candidates.filterNot(_ == cs))).flatten
          }.flatten
        }
      }
      ns.head.map(x => recur(List(x), ns.tail)).flatten
    }
    val result = find(List(triangles, squares, pentagonals, hexgonals, heptagonals, octagonals))
    println(result)
    println(result.head.map(_.toInt).sum)
  }
}