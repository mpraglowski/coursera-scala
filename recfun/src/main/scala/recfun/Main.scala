package recfun
import common._

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = 
    if (c == 0 || c == r ) 1
    else pascal(c-1,r-1)+pascal(c,r-1)

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def balanceIter(opened: Int, chars: List[Char]): Boolean =
      if (opened < 0) false
      else
        if (chars.isEmpty) opened == 0
        else chars.head match {
          case '(' => balanceIter(opened+1, chars.tail)
          case ')' => balanceIter(opened-1, chars.tail)
          case _ => balanceIter(opened, chars.tail)
        }
    balanceIter(0,chars)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    def countChangeIter(money: Int, coins: List[Int]) : Int
      if (money == 0 ) 1
      else if (money < 0) 0
        else if (coins.isEmpty && money >= 1) 0
          else countChangeIter(money, coins.tail) + countChangeIter(money-coins.head, coins)
    countChangeIter(money,coins.sortWith(_ < _))
  }
}
