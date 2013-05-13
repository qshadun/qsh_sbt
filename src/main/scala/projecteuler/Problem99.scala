package projecteuler
//Largest exponential
object Problem99 {
  def numberOfDigits(base: Int, exp: Int) = 1 + (exp * math.log10(base)).toInt
  def main(args: Array[String]): Unit = {
    import scala.io.Source
    val input = Source.fromURL(getClass.getResource("/base_exp.txt")).getLines.map(_.split(',').toList).toList
    val digits = input.map(ns => numberOfDigits(ns(0).toInt, ns(1).toInt)).zipWithIndex
    val max = digits.maxBy(_._1)._1
    println(digits.filter(_._1 == max))
  }
}