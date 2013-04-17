package projecteuler
import scala.io.Source
/**
Problem 42
The nth term of the sequence of triangle numbers is given by, tn =n*(n+1)/2; so the first ten triangle numbers are:

1, 3, 6, 10, 15, 21, 28, 36, 45, 55, ...

By converting each letter in a word to a number corresponding to its alphabetical position and adding these values we form a word value. 
For example, the word value for SKY is 19 + 11 + 25 = 55 = t10. If the word value is a triangle number then we shall call the word a triangle word.

Using words.txt (right click and 'Save Link/Target As...'), a 16K text file containing nearly two-thousand common English words, 
how many are triangle words?
 */
object Problem42 {
  def main(args: Array[String]): Unit = {
    val input = Source.fromURL(getClass.getResource("/words.txt")).mkString.split(",").map(s => s.substring(1, s.length - 1))
    def wordValue(s: String) = s.toCharArray.map(_ - 'A' + 1).sum
    val triNums = (1 to 50).map(x => x * (x + 1) / 2).toSet
    println(input.map(wordValue).filter(triNums.contains).size)
  }
}