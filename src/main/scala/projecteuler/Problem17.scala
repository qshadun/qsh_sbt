package projecteuler
/**
Number letter counts
Problem 17
If the numbers 1 to 5 are written out in words: one, two, three, four, five, then there are 3 + 3 + 5 + 4 + 4 = 19 letters used in total.

If all the numbers from 1 to 1000 (one thousand) inclusive were written out in words, how many letters would be used?


NOTE: Do not count spaces or hyphens. For example, 342 (three hundred and forty-two) contains 23 letters and 115 (one hundred and fifteen) contains 20 letters. The use of "and" when writing out numbers is in compliance with British usage.
 */
object Problem17 {
  val nums = List("one", "two", "three", "four", "five", "six", "seven", "eight", "nine")
  val hundred = "hundred"
  val thousand = "thousand"
  val and = "and"
  val specialTens = List("ten", "eleven", "twelve", "thirteen", "fourteen", "fifteen", "sixteen", "seventeen", "eighteen", "nineteen")
  val tens = List("twenty", "thirty", "forty", "fifty", "sixty", "seventy", "eighty", "ninety")
  
  
  def main(args: Array[String]): Unit = {
    val total = specialTens.map(_.size).sum * 10 +
                hundred.size * 900 +
                thousand.size +
                tens.map(_.size).sum * 10 * 10+
                nums.map(_.size).sum * (9 * 10 + 100 ) +
                "one".size +
                and.size * 891
    println(total)
  }

}