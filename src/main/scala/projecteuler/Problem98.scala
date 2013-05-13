package projecteuler
//Anagramic squares
object Problem98 {
  val digits = List.tabulate(10)(identity)
  val firstDigits = digits.tail
  val lastDigits = List(0, 1, 4, 5, 6, 9) // square last digits can only be these
  val firstAndLastDigits = lastDigits.tail
  def findSquares(wp: Seq[String]): Seq[Seq[Int]] = {
    val letters = wp.head.toCharArray.distinct.toList
    def findMappings(ls: List[Char], used: List[Int], mapped: Map[Char, Int]): List[Map[Char, Int]] = ls match{
      case Nil => List(mapped)
      case head :: tail => 
        val s = head.toString
        val availableDigits = 
          if (wp(0).startsWith(s) || wp(1).startsWith(s) &&
              wp(0).endsWith(s)   || wp(1).endsWith(s))
            firstAndLastDigits
          else if(wp(0).startsWith(s) || wp(1).startsWith(s))
            firstDigits
          else if(wp(0).endsWith(s)   || wp(1).endsWith(s))
            lastDigits
          else
            digits
        availableDigits.filterNot(used.contains).map {d =>
          findMappings(tail, d :: used, mapped + (head -> d))
        }.flatten
    }
    val mappings = findMappings(letters, Nil, Map.empty[Char, Int])
    mappings.map{mapping =>
      val n1 = wp(0).map(mapping).mkString.toInt
      val n2 = wp(1).map(mapping).mkString.toInt
      List(n1, n2)
    }.filter(_.forall(isSquare))
  }
  def isSquare(n: Int): Boolean = {
    val root = math.sqrt(n).toInt
    root * root == n
  }
  def main(args: Array[String]): Unit = {
    import scala.io.Source
    val input = Source.fromURL(getClass.getResource("/words.txt")).mkString.split(",").map(s => s.substring(1, s.length - 1))
    val wordPairs = input.groupBy(_.size).values.map{ls =>
      ls.groupBy(_.toCharArray.sorted.mkString).values.filter(_.size > 1).map{xs =>
        xs.combinations(2)
      }.flatten
    }.flatten
    println(wordPairs.map(_.toList).map(findSquares).filterNot(_.isEmpty).map(ls => ls.map(_.max).max).max)
  }
}