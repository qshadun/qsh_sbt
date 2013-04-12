package projecteuler
/**
Counting Sundays
Problem 19
You are given the following information, but you may prefer to do some research for yourself.

1 Jan 1900 was a Monday.
Thirty days has September,
April, June and November.
All the rest have thirty-one,
Saving February alone,
Which has twenty-eight, rain or shine.
And on leap years, twenty-nine.
A leap year occurs on any year evenly divisible by 4, but not on a century unless it is divisible by 400.

How many Sundays fell on the first of the month during the twentieth century (1 Jan 1901 to 31 Dec 2000)?
 */
object Problem19 {
  def isLeapYear(y: Int) = y % 400 == 0 || (y % 4 == 0 && y % 100 != 0)
  def daysInYear(y: Int) = if (isLeapYear(y)) 366 else 365
  def daysInMonth(y: Int, m: Int) = m match {
    case 2 => if (isLeapYear(y)) 29 else 28
    case x if (List(4, 6, 9, 11).contains(x)) => 30
    case _ => 31
  }
  //calculate all start days in 1901 ~ 2000 count from 1900.1.1
  val startDays = (1901 to 2000).foldLeft(List((1900, 1))){(acc, y) =>
    (y, daysInYear(y - 1) + acc.head._2) :: acc
  }.init
  
  //The days to count is the first day in every month, e.g. Jan 1, Feb 1, ... Dec 1
  val days = startDays.map{x =>
    val year = x._1
    val start = x._2
    (1 to 11).foldLeft(List(start)){(acc, m) =>
      acc.head + daysInMonth(year, m) :: acc
    }.reverse
  }.flatten
  val countOfSundays = days.filter(_ % 7 == 0).size
  
  import java.util.Calendar
  val anotherCounter = (1901 to 2000).map {y =>
    import java.util.Calendar._
    val c = Calendar.getInstance()
    c.set(YEAR, y)
    c.set(DAY_OF_MONTH, 1)
    (JANUARY to DECEMBER).map{m =>
      c.set(MONTH, m)
      if (c.get(DAY_OF_WEEK) == SUNDAY) 1 else 0
    }.sum
  }.sum
  def main(args: Array[String]): Unit = {
    //println(startDays)
    println(countOfSundays)
    println(anotherCounter)
  }

}